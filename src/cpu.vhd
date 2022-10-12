-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Nikita <xmoise01 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
  port (
    CLK   : in std_logic;  -- hodinovy signal
    RESET : in std_logic;  -- asynchronni reset procesoru
    EN    : in std_logic;  -- povoleni cinnosti procesoru
 
    -- synchronni pamet RAM
    DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
    DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
    DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
    DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
    DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
    -- vstupni port
    IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
    IN_VLD    : in std_logic;                      -- data platna
    IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
    -- vystupni port
    OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
    OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
    OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
  );
end cpu;

-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
  signal pc_reg : std_logic_vector(12 downto 0);  -- program counter
  signal pc_ld : std_logic;                       -- load program counter
  signal pc_inc : std_logic;                      -- increment program counter

  signal ireg_reg : std_logic_vector(7 downto 0); -- instruction register
  signal ireg_ld : std_logic;                     -- load instruction register

  type instruction_type is (
    increase_pointer,
    decrease_pointer,
    increase_value,
    decrease_value,
    while_do_start,
    while_do_end,
    do_while_start,
    do_while_end,
    write_value,
    read_value,
    end_of_program
  );
  signal ireg_decoded : instruction_type;          -- decoded instruction

  type fsm_state is (
    state_idle,
    state_fetch,
    state_halt
  );

  signal current_state : fsm_state;               -- current FSM state
  signal next_state : fsm_state;                  -- next FSM state
begin
  -- Program counter process
  pc_counter: process(CLK, RESET, pc_ld, pc_inc)
  begin
    if RESET = '1' then
      pc_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if pc_inc = '1' then
        pc_reg <= pc_reg + 1;
      else
        pc_reg <= (others => '0');
      end if;
    end if;
  end process pc_counter;

  -- DATA_ADDR <= pc_reg when pc_abus = '1' else (others => 'Z');

  -- Instruction register process
  ireg_process: process(CLK, RESET)
  begin
    if RESET = '1' then
      ireg_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if ireg_ld = '1' then
        ireg_reg <= DATA_RDATA;
      end if;
    end if;
  end process ireg_process;

  -- Instruction decoder
  ireg_decoder: process(ireg_reg)
  begin
    case(ireg_reg(7 downto 4)) is
      -- Return (halt) operation
      when x"0" => ireg_decoded <= end_of_program;
      -- do..while operations / Value operations / IO operations
      when x"2" =>
        case(ireg_reg(3 downto 0)) is
          when x"8" => ireg_decoded <= do_while_start;
          when x"9" => ireg_decoded <= do_while_end;
          when x"B" => ireg_decoded <= increase_value;
          when x"D" => ireg_decoded <= decrease_value;
          when x"E" => ireg_decoded <= write_value;
          when x"C" => ireg_decoded <= read_value;
          when others => ireg_decoded <= end_of_program;
        end case; -- Operation second nibble
      -- Pointer operations
      when x"3" =>
        case(ireg_reg(3 downto 0)) is
          when x"E" => ireg_decoded <= increase_pointer;
          when x"C" => ireg_decoded <= decrease_pointer;
          when others => ireg_decoded <= end_of_program;
        end case; -- Operation second nibble
      -- while..do operations
      when x"5" =>
        case ireg_reg(3 downto 0) is
          when x"B" => ireg_decoded <= while_do_start;
          when x"D" => ireg_decoded <= while_do_end;
          when others => ireg_decoded <= end_of_program;
        end case; -- Operation second nibble
      when others => ireg_decoded <= end_of_program;
    end case; -- Operation first nibble
  end process ireg_decoder;
end behavioral;
