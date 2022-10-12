import cocotb, os, hashlib, getpass
from cocotb.clock import Timer, Clock
from cocotb.triggers import Edge, FallingEdge, RisingEdge, Join, with_timeout
from cocotb.queue import Queue
from typing import Optional
from collections.abc import Iterable
from makefun import create_function
import functools, inspect

import logging
logger = logging.getLogger('cocotb.cpu_tb')
loggerlcd = logging.getLogger('cocotb.lcd')
loggerd = logging.getLogger('cocotb.hexdigest')
logger.setLevel(logging.INFO)
loggerlcd.setLevel(logging.INFO)
loggerd.setLevel(logging.INFO)


def enableDebug(all=False, lcd=False):
   logger.setLevel(logging.DEBUG if all else logging.INFO)
   loggerlcd.setLevel(logging.DEBUG if all or lcd else logging.INFO)


ROM_OFS = 0
RAM_OFS = 4096

LCD_WAIT_TIME = 500
KB_WAIT_TIME = 500

async def cpu_lcd(dut, lcd: Optional[list] = None):
    clk = dut.clk
    we = dut.out_we
    busy = dut.out_busy
    data = dut.out_data #cpu output

    busy.value=0

    if lcd is not None: 
      lcd.clear()

    try:
        while True:
          await RisingEdge(clk)
          if we.value == 0: continue
    
          val = chr(int(data.value))
          if lcd is not None:          
            lcd.append(val)
            loggerlcd.debug(f'Characters written to LCD: {repr("".join(lcd))}')

          busy.value = 1
          await Timer(LCD_WAIT_TIME, units='ns')
          await RisingEdge(clk)
          busy.value = 0   
    except Exception as e:
        logger.error(f'LCD module failed with exception {e}')

async def cpu_kb(dut, kbqueue : Queue):
    clk = dut.clk
    req = dut.in_req
    vld = dut.in_vld
    data = dut.in_data #cpu input

    data.value = 0
    vld.value = 0

    try:
        while True:
          await RisingEdge(clk)
          if req.value!=1: continue
    
          logger.debug('Waiting for a key stroke')

          ch = await kbqueue.get()

          data.value = ord(ch)
          vld.value = 1

          while req.value==1:
            await RisingEdge(clk)

          vld.value = 0

          await Timer(KB_WAIT_TIME, units='ns')

    except Exception as e:
        logger.error(f'KB module failed with exception {e}')

async def cpu_ram(dut, mem : list):
    clk = dut.clk
    rdwr = dut.data_rdwr
    en = dut.data_en
    addr = dut.data_addr
    wdata = dut.data_wdata #cpu output
    rdata = dut.data_rdata #cpu input
    
    rdata.value = 0

    assert mem is not None
    loggerd.info(f'code: {hashlib.md5(repr(mem).encode("ascii")).hexdigest()}')

    proglen = 0
    while mem[proglen] > 0: 
        proglen += 1

    try:
        lastma, lastmaw, lastwd = None, None, None
        while True:
            await RisingEdge(clk)

            if en.value == 0:
                continue

            rw, ma = rdwr.value, int(addr.value)

            if rw == 0:
                #cteni
                if lastma != ma:
                  rdata.value = mem[ma]
                  logger.debug(f'Readed value {rdata.value} from address {ma}')
                  lastma = ma

                  if ma == proglen:
                    logger.debug(f'Reached end of program')
                    await Timer(500, units='ns')
                    loggerd.info(f'result: {hashlib.md5(repr(mem).encode("ascii")).hexdigest()}')
                    return (proglen, mem)

            else:
                wd = int(wdata.value)
                if (lastmaw != ma) or (lastwd != wd):
                  mem[ma] = wd
                  lastwd = wd
                  lastmaw = ma
                  if lastma == lastmaw: lastma = None
                  logger.debug(f'Write value {wd} to address {ma}')

    except Exception as e:
        logger.error(f'RAM module failed with exception {e}')
        return (proglen, mem)

async def cpu_dut_init(dut):
    dut.clk.value=0
    dut.en.value=0
    dut.reset.value=0
    dut.data_rdata.value=0
    dut.in_data.value=0
    dut.in_vld.value=0
    dut.out_busy.value=0
    
    clk_100mhz = Clock(dut.clk, 10, units='ns')
    clk_gen = cocotb.start_soon(clk_100mhz.start())

    return clk_gen

async def run_program(dut, prog : str, timeout_ns : int = 1000, kb_data : Optional[str] = None, mem_data : Optional[str] = None):
    clk_gen = await cpu_dut_init(dut)

    mem = [0 for i in range(8192)]

    if mem_data is not None:
        for i, ch in enumerate(mem_data):
            mem[RAM_OFS+i] = ord(ch)

    for i, ch in enumerate(prog):
        mem[ROM_OFS+i] = ord(ch)

    meminst = cocotb.start_soon(cpu_ram(dut, mem=mem))

    lcd = []
    lcdnst = cocotb.start_soon(cpu_lcd(dut, lcd))

    kbqueue = Queue[str]()
    kbinst = cocotb.start_soon(cpu_kb(dut, kbqueue))

    dut.reset.value=1
    await Timer(50, units='ns')
    dut.reset.value = 0

    logger.debug('reset done')

    await FallingEdge(dut.clk)
    dut.en.value=1

    logger.debug('cpu enabled')

    if kb_data is not None:
        for ch in kb_data:
            await kbqueue.put(ch)
    
    proglen, mem = await with_timeout(meminst, timeout_ns, 'ns')
    return proglen, mem, ''.join(lcd)

def tb_test(*args,**kwargs):
    def recr(fg):
        uid = getpass.getuser()
        h = hashlib.md5((uid+':'+inspect.getsource(fg)).encode("ascii")).hexdigest()
        @functools.wraps(fg)
        async def ff(dut):
            loggerd.info(f'test: {h}')
            return await fg(dut, uid)
        @functools.wraps(fg)
        async def fff(dut):
            loggerd.info(f'test: {h}')
            return await fg(dut)
        fname = ff.__name__
        if fname.endswith('_login'):
            fff = create_function(f'{fname}_{uid}(dut)', ff)
        return cocotb.test(*args,**kwargs)(fff)
    return recr   

with open(__file__,'r') as f:
    loggerd.info(f'lib: {hashlib.md5(f.read().encode("ascii")).hexdigest()}')

if __name__=="__main__":
  # call the main function
  print("Run the simulation by calling make.")
  exit()
