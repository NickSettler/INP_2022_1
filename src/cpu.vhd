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
  signal pc_reg : std_logic_vector(12 downto 0);      -- program counter
  signal pc_ld : std_logic;                           -- load program counter
  signal pc_inc : std_logic;                          -- increment program counter
  signal pc_abus : std_logic;                         -- program counter to address bus

  signal ireg_reg : std_logic_vector(7 downto 0);     -- instruction register
  signal ireg_ld : std_logic;                         -- load instruction register

  signal pointer_reg : std_logic_vector(12 downto 0); -- pointer register
  signal pointer_inc : std_logic;                     -- increment pointer register
  signal pointer_dec : std_logic;                     -- decrement pointer register

  signal addr_mx : std_logic_vector(12 downto 0);     -- address mux
  signal addr_mx_sel : std_logic;                     -- select data address to work with
                                                      -- PC [0] / PTR [1]

  signal wdata_mx : std_logic_vector(7 downto 0);     -- mux output
                                                      -- IN_DATA [00] / *pointer_reg + 1 [01] / *pointer_reg - 1 [10]
  signal wdata_mx_sel : std_logic_vector(1 downto 0); -- mux selector

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
    state_fetch_0,
    state_fetch_1,
    state_decode,
    state_halt,
    state_increase_pointer,
    state_decrease_pointer,
    state_increament_value_0,
    state_increament_value_1,
    state_increament_value_2
  );                                              -- Finite State Machine states

  signal current_state : fsm_state;               -- current FSM state
  signal next_state : fsm_state;                  -- next FSM state
begin
  -- Program counter process
  pc_counter: process(CLK, RESET, pc_inc)
  begin
    if RESET = '1' then
      pc_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if pc_inc = '1' then
        pc_reg <= pc_reg + 1;
      end if;
    end if;
  end process pc_counter;

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

  pointer_process: process(CLK, RESET, pointer_inc, pointer_dec)
  begin
    if RESET = '1' then
      pointer_reg <= ('1', others => '0');
    elsif rising_edge(CLK) then
      if pointer_inc = '1' then
        case(pointer_reg(8 downto 0)) is
          when "111111111" => pointer_reg <= pointer_reg(12 downto 9) & "000000000";
          when others => pointer_reg <= pointer_reg + 1;
        end case;
      elsif pointer_dec = '1' then
        case(pointer_reg(8 downto 0)) is
          when "000000000" => pointer_reg <= pointer_reg(12 downto 9) & "111111111";
          when others => pointer_reg <= pointer_reg - 1;
        end case;
      end if;
    end if;
  end process pointer_process;

  -- DATA_ADDR <= pointer_reg;
  OUT_DATA <= DATA_RDATA;

  addr_mx_process: process(CLK, RESET, addr_mx_sel)
  begin
    if RESET = '1' then
      -- addr_mx_sel <= '0';
      addr_mx <= (others => '0');
    elsif rising_edge(CLK) then
      if addr_mx_sel = '0' then
        addr_mx <= pc_reg;
      elsif addr_mx_sel = '1' then
        addr_mx <= pointer_reg;
      end if;
    end if;
  end process addr_mx_process;

  DATA_ADDR <= addr_mx;

  wdata_mx_process: process(CLK, RESET, wdata_mx_sel)
  begin
    if RESET = '1' then
      wdata_mx <= (others => '0');
    elsif rising_edge(CLK) then
      case wdata_mx_sel is
        when "00" => wdata_mx <= IN_DATA;
        when "01" => wdata_mx <= DATA_RDATA + 1;
        when "10" => wdata_mx <= DATA_RDATA - 1;
        when others => wdata_mx <= (others => '0');
      end case;
    end if;
  end process wdata_mx_process;

  DATA_WDATA <= wdata_mx;

  -- ------------------------------------------------------------
  -- Instructions list
  -- ------------------------------------------------------------
  -- 0x3E - increase pointer
  -- 0x3C - decrease pointer
  -- 0x2B - increase value at pointer
  -- 0x2D - decrease value at pointer
  -- 0x5B - start while..do loop if value at pointer is not zero
  -- 0x5D - end while..do loop if value at pointer is not zero
  -- 0x28 - start do..while loop
  -- 0x29 - end do..while loop if value at pointer is not zero
  -- 0x2E - write value at pointer to LCD
  -- 0x2C - read value from keyboard to pointer
  -- 0x00 - end of program
  -- ------------------------------------------------------------

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

  -- Current FSM state process
  fsm_current_state_process: process(CLK, RESET)
  begin
    if RESET = '1' then
      current_state <= state_idle;
    elsif rising_edge(CLK) and EN = '1' then
      current_state <= next_state;
    end if;
  end process fsm_current_state_process;

  -- Next FSM state process
  fsm_next_state_process: process(current_state, ireg_decoded)
  begin
    IN_REQ <= '0';

    OUT_WE <= '0';

    DATA_EN <= '0';
    DATA_RDWR <= '0';

    pc_inc <= '0';
    pc_ld <= '0';
    pc_abus <= '0';

    ireg_ld <= '0';

    pointer_inc <= '0';
    pointer_dec <= '0';

    -- addr_mx_sel <= '0';
    wdata_mx_sel <= "00";

    case current_state is
      -- Idle FSM state
      when state_idle =>
        next_state <= state_fetch_0;
      -- Fetch FSM state 1 (F)
      when state_fetch_0 =>
        DATA_EN <= '1';
        next_state <= state_fetch_1;
      when state_fetch_1 =>
        ireg_ld <= '1';
        next_state <= state_decode;
      -- Decode FSM state (D)
      when state_decode =>
        case(ireg_decoded) is
          when end_of_program => next_state <= state_halt;
          when increase_pointer => next_state <= state_increase_pointer;
          when decrease_pointer => next_state <= state_decrease_pointer;
          when increase_value => next_state <= state_increament_value_0;
          when others => next_state <= state_halt;
        end case;
      when state_increase_pointer =>
        pointer_inc <= '1';
        pc_inc <= '1';

        next_state <= state_fetch_0;
      when state_decrease_pointer =>
        pointer_dec <= '1';
        pc_inc <= '1';

        next_state <= state_fetch_0;

      -- Set address MX to PTR
      when state_increament_value_0 =>
        addr_mx_sel <= '1';
        next_state <= state_increament_value_1;
      -- Read data at mem[PTR]
      when state_increament_value_1 =>
        DATA_EN <= '1';
        next_state <= state_increament_value_2;
      -- Increment mem[PTR] by one and increase PC by one
      when state_increament_value_2 =>
        wdata_mx_sel <= "01";
        pc_inc <= '1';
        next_state <= state_increament_value_3;
      -- Store incremented mem[PTR]
      when state_increament_value_3 =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';

        addr_mx_sel <= '0';
        next_state <= state_fetch_0;
      when others =>
        next_state <= state_halt;
    end case;
  end process fsm_next_state_process;
end behavioral;
