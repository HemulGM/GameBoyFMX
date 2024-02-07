unit GBCpu;

interface

uses
  GBMemory, GBInterruptManager, GBTimer, GBGpu, System.SysUtils, System.Classes,
  GBSound;

type
  {TGBGpuNew = class

  end;
  PGBGpuNew = ^TGBGpuNew;      }
  TGBCpu = class
  private
    // GB CPU lr35902 的寄存器
    // 前8位是A，后8位是F，分开可读，同时用叫AF寄存器
    Reg_A: Byte;
    Reg_F: Byte;
    // 前8位是B，后8位是C，分开可读，同时用叫BC寄存器
    Reg_B: Byte;
    Reg_C: Byte;
    // 前8位是D，后8位是E，分开可读，同时用叫DE寄存器
    Reg_D: Byte;
    Reg_E: Byte;
    // 前8位是H，后8位是L，分开可读，同时用叫HL寄存器
    Reg_H: Byte;
    Reg_L: Byte;

    // cpu halt标识
    isHalted: Boolean; // = False;

    // 中断标识
    pendingInterruptEnable: Boolean;
    // 从内存管理器中获取opcode
    function getOpcode(): Integer;
    // decode opcode
    procedure deCode(opcode: Integer);

    // 将dec指令归类放同一个
    procedure dec_opcode(opcode: Byte);
    procedure jrcc_opcode(opcode: Byte);
    procedure daa_opcode(opcode: Byte);

    procedure add16_opcode(opcode: Byte);
    procedure add_opcode(opcode: Byte);
    procedure adc_opcode(opcode: Byte);
    procedure sub_opcode(opcode: Byte);
    procedure sbc_opcode(opcode: Byte);
    procedure and_opcode(opcode: Byte);
    procedure xor_opcode(opcode: Byte);
    procedure or_opcode(opcode: Byte);
    procedure load_opcode(opcode: Byte);
    procedure cp_opcode(opcode: Byte);
    procedure retcc_opcode(opcode: Byte);
    procedure jpcc_opcpode(opcode: Byte);
    procedure callcc_opcode(opcode: Byte);
    procedure rst_opcode(opcode: Byte);
    //重写部分opcode的处理过程
    procedure inc16_opcode(opcode: Byte);
    procedure dec16_opcode(opcode: Byte);
    procedure inc_opcode(opcode: Byte);
    procedure jmp_opcode(opcode: Byte);

    function popHelper(): Integer;
    procedure pushHelper(value: Integer);
    procedure retHelper();
    function swapHelper(value: Integer): Integer;
    function cbHelperRead(opcode: Integer): Integer;
    procedure cbHelperWrite(opcode, value: Integer);
    // PreFix CB部分的opcode，都是类似$CB00，所以要用Word类型
    procedure rlc_opcode(opcode: Word);
    procedure rrc_opcode(opcode: Word);
    procedure rl_opcode(opcode: Word);
    procedure rr_opcode(opcode: Word);
    procedure sla_opcode(opcode: Word);
    procedure sra_opcode(opcode: Word);
    procedure swap_opcode(opcode: Word);
    procedure srl_opcode(opcode: Word);
    procedure bit_opcode(opcode: Word);
    procedure res_opcode(opcode: Word);
    procedure set_opcode(opcode: Word);

    // 处理
    procedure processEi(opcode: Integer);
    procedure processInterrupts();
  public
    // 栈指针寄存器 16位
    Reg_SP: Word;
    // 程序指针寄存器 16位，最大可寻FFFF=65535，即64M的空间
    Reg_PC: Word;
    // 标志位寄存器，本身是8位，但就用高4位，模拟时分开写
    // 设计错误，不应该分开，其实4个标识位寄存器，是放在F寄存器的。
    // 并且是根据位，来识别True or False，取高4位，低4位保持为0
//    Flag_Z: Boolean; // Zero Flag
//    Flag_N: Boolean; // Subtract Flag
//    Flag_H: Boolean; // Half Carry Flag
//    Flag_C: Boolean; // Carry Flag

    CPU_Ticks: Integer;

    _debug_opcode: Integer;
    GBInterrupt: TGBInterruptManager;
    GBTimer: TGBTimer;

//    GBMem: TGBMemory;
    _pGBMem: PGBMemory;
//    GBGpu: TGBGpu;
    _pGBGpu: PGBGpu;
    //_pGBGpuNew:PGBGpuNew;
    _pGBSound: PGBSound;
    // cpu暂时标识
    Paused: Boolean; //= False;
    _instr_count: Integer;
    _icycle_count: Integer;
    _debug_log: string;
    _debug_log_list: TStringlist;
    useNewGPU: Boolean;
    // 寄存器获取
    function getReg_A(): Byte;
    function getReg_F(): Byte;
    function getReg_AF(): Word;
    function getReg_B(): Byte;
    function getReg_C(): Byte;
    function getReg_BC(): Word;
    function getReg_D(): Byte;
    function getReg_E(): Byte;
    function getReg_DE(): Word;
    function getReg_H(): Byte;
    function getReg_L(): Byte;
    function getReg_HL(): Word;
    function getReg_SP_Low(): Byte;
    function getReg_SP_High(): Byte;
    // 寄存器赋值
    procedure setReg_A(val: Byte);
    procedure setReg_F(val: Byte);
    procedure setReg_AF(val: Word);
    procedure setReg_B(val: Byte);
    procedure setReg_C(val: Byte);
    procedure setReg_BC(val: Word);
    procedure setReg_D(val: Byte);
    procedure setReg_E(val: Byte);
    procedure setReg_DE(val: Word);
    procedure setReg_H(val: Byte);
    procedure setReg_L(val: Byte);
    procedure setReg_HL(val: Word);

    //寄存器SP和PC的INC要手动处理

    procedure pop_opcode(opcode: Integer);
    procedure push_opcode(opcode: Integer);

    constructor Create(pgbMemory: PGBMemory; pgbGpu: PGBGpu; pgbSound: PGBSound); overload;
    //Constructor Create(pgbMemory: PGBMemory;pgbGpu: PGBGpuNew; pgbSound: PGBSound); overload;

    // 用set/get实现，都操作寄存器F(Reg_F);
    procedure setFlag_Z(value: Boolean);
    function getFlag_Z(): Boolean;
    procedure setFlag_N(value: Boolean);
    function getFlag_N(): Boolean;
    procedure setFlag_H(value: Boolean);
    function getFlag_H(): Boolean;
    procedure setFlag_C(value: Boolean);
    function getFlag_C(): Boolean;

    // 消耗时钟周期,待完成
    procedure consumeClockCycles(cycles: Integer);

    procedure step(i: Integer);
    procedure skipBios();
    procedure main();

  end;

implementation

{ TGBCpu }

procedure TGBCpu.adc_opcode(opcode: Byte);
var
  second, oldval, result: Integer;
begin
  case opcode of
    $8F:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $88:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $89:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $8A:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $8B:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $8C:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $8D:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $8E:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $CE:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  oldval := getReg_A;
  result := oldval + second;
  if getFlag_C then
    result := result + 1;
  setFlag_N(False);
  if result > 255 then
  begin
    setFlag_C(True);
    result := result and $FF;
  end
  else
  begin
    setFlag_C(False);
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if ((getReg_A xor second xor result) and $10) <> 0 then
    setFlag_H(True)
  else
    setFlag_H(False);
  setReg_A(result);

  if (opcode = $8E) or (opcode = $CE) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.add16_opcode(opcode: Byte);
var
  value, result, hl, resultXor: Integer;
begin
//        Register destReg = null;
  case opcode of
    $09:
      begin
        value := getReg_BC;
//      consumeClockCycles(8);
      end;
    $19:
      begin
        value := getReg_DE;
//      consumeClockCycles(8);
      end;
    $29:
      begin
        value := getReg_HL;
//      consumeClockCycles(8);
      end;
    $39:
      begin
        value := Reg_SP;
//      consumeClockCycles(8);
      end;
    $E8:
      begin
        value := _PGBMem^.ReadGBMemory(Reg_PC);
        Reg_PC := Reg_PC + 1;
        if value > 127 then
          value := -((not value + 1) and 255);
//      consumeClockCycles(16);
      end;
  end;
  hl := getReg_HL;
  if opcode = $E8 then
  begin
    result := value + Reg_SP;
    setFlag_Z(False);
    setFlag_N(False);
    resultXor := Reg_SP xor value xor result;
    if (resultXor and $10) <> 0 then
      setFlag_H(True)
    else
      setFlag_H(False);
    if (resultXor and $100) <> 0 then
      setFlag_C(True)
    else
      setFlag_C(False);
    result := result and $FFFF;
    Reg_SP := result;
  end
  else
  begin
    result := hl + value;
    setFlag_N(False);
    if (((hl and $0FFF) + (value and $0FFF)) and $1000) <> 0 then
      setFlag_H(True)
    else
      setFlag_H(False);
    if result > $FFFF then
    begin
      setFlag_C(True);
      result := result and $FFFF;
    end
    else
    begin
      setFlag_C(False);
    end;
    setReg_HL(result);

    if (opcode = $E8) then
      consumeClockCycles(16)
    else
      consumeClockCycles(8);
  end;
end;

procedure TGBCpu.add_opcode(opcode: Byte);
var
  second, oldval, result: Integer;
begin
  case opcode of
    $87:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $80:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $81:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $82:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $83:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $84:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $85:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $86:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $C6:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  oldval := getReg_A;
  result := oldval + second;
  setFlag_N(False);
  if result > 255 then
  begin
    setFlag_C(True);
    result := result and $FF;
  end
  else
  begin
    setFlag_C(False);
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if (oldval and $F) + ($F and second) > $F then
    setFlag_H(True)
  else
    setFlag_H(False);
  setReg_A(result);

  if (opcode = $86) or (opcode = $C6) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.and_opcode(opcode: Byte);
var
  second: Integer;
begin
  case opcode of
    $A7:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $A0:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $A1:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $A2:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $A3:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $A4:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $A5:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $A6:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $E6:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  setReg_A(getReg_A and second);
  if (getReg_A = 0) then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(True);
  setFlag_C(False);

  if (opcode = $A6) or (opcode = $E6) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.bit_opcode(opcode: Word);
var
  value, bitIndex: Integer;
begin
  value := cbHelperRead(opcode);
  bitIndex := (opcode - $CB40) div 8;
  if ((value shr bitIndex) and 1) = 1 then
    setFlag_Z(False)
  else
    setFlag_Z(True);
  setFlag_H(True);
  setFlag_N(False);
end;

procedure TGBCpu.callcc_opcode(opcode: Byte);
var
  condition: Boolean;
  address, temp: Integer;
begin
  case opcode of
    $C4:
      begin
        condition := not getFlag_Z;
      end;
    $CC:
      begin
        condition := getFlag_Z;
      end;
    $D4:
      begin
        condition := not getFlag_C;
      end;
    $DC:
      begin
        condition := getFlag_C;
      end;
  end;
  address := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  temp := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  if not condition then
  begin
    consumeClockCycles(12);
  end
  else
  begin
    pushHelper(Reg_PC);
    temp := temp shl 8;
    address := address or temp;
    Reg_PC := address;
    consumeClockCycles(24);
  end;
end;

function TGBCpu.cbHelperRead(opcode: Integer): Integer;
var
  ret: Integer;
begin
  ret := 0;
  case opcode and $0F of
    $07, $0F:
      begin
        ret := getReg_A;
//      consumeClockCycles(8);
      end;
    $00, $08:
      begin
        ret := getReg_B;
//      consumeClockCycles(8);
      end;
    $01, $09:
      begin
        ret := getReg_C;
//      consumeClockCycles(8);
      end;
    $02, $0A:
      begin
        ret := getReg_D;
//      consumeClockCycles(8);
      end;
    $03, $0B:
      begin
        ret := getReg_E;
//      consumeClockCycles(8);
      end;
    $04, $0C:
      begin
        ret := getReg_H;
//      consumeClockCycles(8);
      end;
    $05, $0D:
      begin
        ret := getReg_L;
//      consumeClockCycles(8);
      end;
    $06, $0E:
      begin
        ret := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(16);
      end;
  end;
  result := ret;
end;

procedure TGBCpu.cbHelperWrite(opcode, value: Integer);
begin
  case opcode and $0F of
    $7, $F:
      begin
        setReg_A(value);
        consumeClockCycles(8);
      end;
    $0, $8:
      begin
        setReg_B(value);
        consumeClockCycles(8);
      end;
    $1, $9:
      begin
        setReg_C(value);
        consumeClockCycles(8);
      end;
    $2, $A:
      begin
        setReg_D(value);
        consumeClockCycles(8);
      end;
    $3, $B:
      begin
        setReg_E(value);
        consumeClockCycles(8);
      end;
    $4, $C:
      begin
        setReg_H(value);
        consumeClockCycles(8);
      end;
    $5, $D:
      begin
        setReg_L(value);
        consumeClockCycles(8);
      end;
    $6, $E:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, value);
        consumeClockCycles(16);
      end;
  end;
end;

procedure TGBCpu.consumeClockCycles(cycles: Integer);
var
  I: Integer;
begin
//  Inc(_icycle_count);

//        if _icycle_count = 61138 then
//         _icycle_count :=61138 ;
// 消耗时钟周期,待完成
  GBTimer.Instance.step(cycles);
// GPU还是要完成一下。
  if useNewGPU then
   // _pGBGpuNew.updateLCDStatus(cycles)

  else
    _pGBGpu^.step(cycles);

//// 处理声音
//  if (_pGBSound^._debug_i>0) and (_pGBSound^._debug_i<144) then
////    if (_icycle_count>75342) and (_icycle_count<76342) then
//       _debug_log_list.Add('I:'+IntToStr(_icycle_count)+':OPCODE:'+IntToStr(_debug_opcode)+
//                  ':SoundT:'+IntToStr(_pGBSound^._debug_i)+':Cycle:'+IntToStr(cycles)+
////                  ':REG_F:'+IntToStr(Reg_F)+':REG_A:'+IntToStr(Reg_A)+':PC:'
////                  +IntToStr(Reg_PC)+':LY:'+IntToStr(_pGBGpu^.line)+':MCLOCK:'
////                  +IntToStr(_pGBGpu^.modeClock)+':LCDMODE:'+IntToStr(Ord(_pGBGpu^.currentMode)));
//                  ':CN1:'+ IntToStr(_pGBSound^.channel1.getVolume.getIndex));
//  if _pGBSound^._debug_i=144 then
////  if _icycle_count=76342 then
//  begin
//      _debug_log_list.SaveToFile('D:\gbsound\wjlsnd.txt');
//     _debug_log_list.Add(IntToStr(_pGBSound^._debug_i)+':'+IntToStr(cycles));
//
//  end;


  if _pGBSound <> nil then
    _pGBSound^.updateSound(cycles);
end;

procedure TGBCpu.cp_opcode(opcode: Byte);
var
  second, result: Integer;
begin
  case opcode of
    $bf:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $b8:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $b9:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $ba:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $bb:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $bc:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $bd:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $be:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $fe:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  result := getReg_A - second;
  setFlag_N(True);
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if second > getReg_A then
    setFlag_C(True)
  else
    setFlag_C(False);
  if (getReg_A and $0F) < ($0F and second) then
    setFlag_H(True)
  else
    setFlag_H(False);

  if (opcode = $BE) or (opcode = $FE) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;
        {
constructor TGBCpu.Create(pgbMemory: PGBMemory; pgbGpu: PGBGpuNew;
  pgbSound: PGBSound);
begin
  useNewGPU := True;
   _PGBMem := @pgbMemory^;
   _pGBGpuNew := @pgbGpu^;
   _pGBSound := @pgbSound^;

   setReg_A(0);
   setReg_B(0);
   setReg_C(0);
   setReg_D(0);
   setReg_E(0);
   setReg_H(0);
   setReg_L(0);
   Reg_PC := 0;
   Reg_SP := 0;
  _debug_log := '';
  _debug_log_list := TStringList.Create;
  _instr_count := 0;
  _icycle_count := 0;
end; }

constructor TGBCpu.Create(pgbMemory: pgbMemory; pgbGpu: pgbGpu; pgbSound: pgbSound);
begin
  useNewGPU := False;
  _PGBMem := @pgbMemory^;
  _pGBGpu := @pgbGpu^;
  _pGBSound := @pgbSound^;
//   GBGpu := @pgbGpu;
  setReg_A(0);
  setReg_B(0);
  setReg_C(0);
  setReg_D(0);
  setReg_E(0);
  setReg_H(0);
  setReg_L(0);
  Reg_PC := 0;
  Reg_SP := 0;
  _debug_log := '';
  _debug_log_list := TStringList.Create;
  _instr_count := 0;
  _icycle_count := 0;
end;

procedure TGBCpu.daa_opcode(opcode: Byte);
var
  op: Integer;
begin
  op := getReg_A();
  if (not getFlag_N) then
  begin
    if (getFlag_H) or ((op and $F) > 9) then
    begin
      op := op + $06;
    end;
    if (getFlag_C) or (op > $9F) then
    begin
      op := op + $60;
    end;
  end
  else
  begin
    if (getFlag_H) then
    begin
      op := (op - 6) and $FF;
    end;
    if (getFlag_C) then
    begin
      op := op - $60;
    end;
  end;
  setFlag_H(False);
  setFlag_Z(False);
  if (op and $100) = $100 then
  begin
    setFlag_C(True);
  end;
  op := op and $FF;
  if op = 0 then
    setFlag_Z(True);
  setReg_A(op and $FF);
  consumeClockCycles(4);
end;

procedure TGBCpu.dec16_opcode(opcode: Byte);
begin

end;

procedure TGBCpu.deCode(opcode: Integer);
var
  tmp, value: Integer;
begin
  case opcode of
    $00:
      begin// NOP
        consumeClockCycles(4);
      end;
    $76:
      begin// HALT
        isHalted := true;
        consumeClockCycles(4);
      end;
    $10:
      begin// STOP 0
        consumeClockCycles(4);
      end;
    $F3:
      begin// DI
        GBInterrupt.Instance.masterDisable;
        consumeClockCycles(4);
      end;
    $FB:
      begin// EI
    // 开启中断
        pendingInterruptEnable := True;
        consumeClockCycles(4);
      end;
    $03:
      begin// INC BC
        setReg_BC(getReg_BC() + 1);
        consumeClockCycles(8);
      end;
    $13:
      begin// INC DE
        setReg_DE(getReg_DE() + 1);
        consumeClockCycles(8);
      end;
    $23:
      begin// INC HL
        setReg_HL(getReg_HL() + 1);
        consumeClockCycles(8);
      end;
    $33:
      begin// INC SP
        Inc(Reg_SP);
      // 原理论错误，这里不需要执行8周期....bug fix.
        consumeClockCycles(8);
      end;
    $0b:
      begin// DEC BC
        setReg_BC(getReg_BC() - 1);
        consumeClockCycles(8);
      end;
    $1b:
      begin// DEC DE
        setReg_DE(getReg_DE() - 1);
        consumeClockCycles(8);
      end;
    $2b:
      begin// DEC HL
        setReg_HL(getReg_HL() - 1);
        consumeClockCycles(8);
      end;
    $3b:
      begin// DEC SP
        Dec(Reg_SP);
       // 原理论错误，这里不需要执行8周期....bug fix
        consumeClockCycles(8);
      end;
    $04, $0c, $14, $1c, $24, $2c, $34, $3c:
      begin
        inc_opcode(opcode);
      end;
    $05, $0d, $15, $1d, $25, $2d, $35, $3d:
      begin
        dec_opcode(opcode);
      end;
    $07:
      begin// RLCA
        value := getReg_A();
        tmp := (value shl 1) or (value shr 7);
        tmp := tmp and 255;
        setReg_A(tmp);
        setFlag_Z(False);
        setFlag_N(False);
        setFlag_H(False);
        if ((value and $80) <> 0) then
        begin
          setFlag_C(True);
        end
        else
        begin
          setFlag_C(False);
        end;
        consumeClockCycles(4);
      end;
    $0f:
      begin// RRCA
        value := getReg_A();
        tmp := value and $01;
        value := value shr 1;
        setReg_A(value or (tmp shl 7));
        if (tmp <> 0) then
        begin
          setFlag_C(True);
        end
        else
        begin
          setFlag_C(False);
        end;
        setFlag_Z(False);
        setFlag_N(False);
        setFlag_H(False);
        consumeClockCycles(4);
      end;
    $17:
      begin// RLA
        value := getReg_A();
        if getFlag_C then
          tmp := 1
        else
          tmp := 0;
        setReg_A(((value shl 1) or tmp) and $FF);
        if ((value shr 7) <> 0) then
        begin
          setFlag_C(True);
        end
        else
        begin
          setFlag_C(False);
        end;
        setFlag_Z(False);
        setFlag_N(False);
        setFlag_H(False);
        consumeClockCycles(4);
      end;
    $1f:
      begin// RRA
        value := getReg_A();
        if getFlag_C then
          tmp := 1
        else
          tmp := 0;
        setReg_A((value shr 1) or (tmp shl 7));
        if ((value and $01) = 1) then
          setFlag_C(True)
        else
          setFlag_C(False);
        setFlag_Z(False);
        setFlag_N(False);
        setFlag_H(False);
        consumeClockCycles(4);
      end;
    $18:
      begin// JR r8
        value := _PGBMem^.ReadGBMemory(Reg_PC);
        Reg_PC := Reg_PC + 1;
        tmp := Reg_PC;
        Reg_PC := Reg_PC + 1;
        if (value > 127) then
        begin
          value := -((not value + 1) and 255); //
        end;
        tmp := tmp + value;
        Reg_PC := tmp;
        consumeClockCycles(12);
      end;
    $e9:
      begin// JP (HL)
        Reg_PC := getReg_HL();
        consumeClockCycles(4);
      end;
    $20, $28, $30, $38:
      begin
        jrcc_opcode(opcode);
      end;
    $27:
      begin// DAA
        daa_opcode(opcode);
      end;
    $2F:
      begin// CPL
        setReg_A(not getReg_A() and 255);
        setFlag_N(True);
        setFlag_H(True);
        consumeClockCycles(4);
      end;
    $37:
      begin// SCF
        setFlag_N(False);
        setFlag_H(False);
        setFlag_C(True);
        consumeClockCycles(4);
      end;
    $3F:
      begin// CCF
        setFlag_N(False);
        setFlag_H(False);
        if getFlag_C then
          setFlag_C(False)
        else
          setFlag_C(True);
        consumeClockCycles(4);
      end;
    $C9:
      begin// RET
        Reg_PC := popHelper();
        consumeClockCycles(16);
      end;
    $CD:
      begin// CALL a16
        value := _PGBMem^.ReadGBMemory(Reg_PC);
        Reg_PC := Reg_PC + 1;
        tmp := _PGBMem^.ReadGBMemory(Reg_PC);
        Reg_PC := Reg_PC + 1;
        tmp := tmp shl 8;
        value := value or tmp;
        pushHelper(Reg_PC);
        Reg_PC := value;
        consumeClockCycles(24); // CD指令是24周期，原写成12
      end;
    $D9:
      begin// RETI
        retHelper();
        GBInterrupt.Instance.masterEnable;
        consumeClockCycles(16);
      end;
    $39, $29, $19, $09, $E8:
      begin
        add16_opcode(opcode);
      end;
    $C1, $D1, $E1, $F1:
      begin
        pop_opcode(opcode);
      end;
    $C5, $D5, $E5, $F5:
      begin
        push_opcode(opcode);
      end;
    $01, $02, $06, $08, $0a, $0e, $16, $1a, $1e, $21, $22, $26, $2a, $2e, $31, $32, $36, $3a, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a, $5b, $5c, $5d, $5e, $5f, $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f, $70, $71, $72, $73, $74, $75, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f, $f8, $f9, $fa, $3e, $f2, $e0, $e2, $ea, $f0, $11, $12:
      begin
        load_opcode(opcode);
      end;
    $80, $81, $82, $83, $84, $85, $86, $87, $c6:
      begin
        add_opcode(opcode);
      end;
    $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $ce:
      begin
        adc_opcode(opcode);
      end;
    $90, $91, $92, $93, $94, $95, $96, $97, $d6:
      begin
        sub_opcode(opcode);
      end;
    $98, $99, $9a, $9b, $9c, $9d, $9e, $9f, $de:
      begin
        sbc_opcode(opcode);
      end;
    $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $e6:
      begin
        and_opcode(opcode);
      end;
    $a8, $a9, $aa, $ab, $ac, $ad, $ae, $af, $ee:
      begin
        xor_opcode(opcode);
      end;
    $b0, $b1, $b2, $b3, $b4, $b5, $b6, $b7, $f6:
      begin
        or_opcode(opcode);
      end;
    $b8, $b9, $ba, $bb, $bc, $bd, $be, $bf, $fe:
      begin
        cp_opcode(opcode);
      end;
    $c0, $c8, $d0, $d8:
      begin
        retcc_opcode(opcode);
      end;
    $c2, $ca, $d2, $da:
      begin
        jpcc_opcpode(opcode);
      end;
    $C3:
      begin// JMP
        jmp_opcode($C3);
      end;
    $c4, $cc, $d4, $dc:
      begin
        callcc_opcode(opcode);
      end;
    $d3, $db, $dd, $e3, $e4, $eb, $ec, $ed, $f4, $fc, $fd:
      begin
      //opcode not support!!!
      end;
    $c7, $cf, $d7, $df, $e7, $ef, $ff, $f7:
      begin
        rst_opcode(opcode);
      end;
    $cb00, $cb01, $cb02, $cb03, $cb04, $cb05, $cb06, $cb07:
      begin
        rlc_opcode(opcode);
      end;
    $cb08, $cb09, $cb0a, $cb0b, $cb0c, $cb0d, $cb0e, $cb0f:
      begin
        rrc_opcode(opcode);
      end;
    $cb10, $cb11, $cb12, $cb13, $cb14, $cb15, $cb16, $cb17:
      begin
        rl_opcode(opcode);
      end;
    $cb18, $cb19, $cb1a, $cb1b, $cb1c, $cb1d, $cb1e, $cb1f:
      begin
        rr_opcode(opcode);
      end;
    $cb20, $cb21, $cb22, $cb23, $cb24, $cb25, $cb26, $cb27:
      begin
        sla_opcode(opcode);
      end;
    $cb28, $cb29, $cb2a, $cb2b, $cb2c, $cb2d, $cb2e, $cb2f:
      begin
        sra_opcode(opcode);
      end;
    $cb30, $cb31, $cb32, $cb33, $cb34, $cb35, $cb36, $cb37:
      begin
        swap_opcode(opcode);
      end;
    $cb38, $cb39, $cb3a, $cb3b, $cb3c, $cb3d, $cb3e, $cb3f:
      begin
        srl_opcode(opcode);
      end;
    $cb40, $cb41, $cb42, $cb43, $cb44, $cb45, $cb46, $cb47, $cb48, $cb49, $cb4a, $cb4b, $cb4c, $cb4d, $cb4e, $cb4f, $cb50, $cb51, $cb52, $cb53, $cb54, $cb55, $cb56, $cb57, $cb58, $cb59, $cb5a, $cb5b, $cb5c, $cb5d, $cb5e, $cb5f, $cb60, $cb61, $cb62, $cb63, $cb64, $cb65, $cb66, $cb67, $cb68, $cb69, $cb6a, $cb6b, $cb6c, $cb6d, $cb6e, $cb6f, $cb70, $cb71, $cb72, $cb73, $cb74, $cb75, $cb76, $cb77, $cb78, $cb79, $cb7a, $cb7b, $cb7c, $cb7d, $cb7e, $cb7f:
      begin
        bit_opcode(opcode);
      end;
    $cb80, $cb81, $cb82, $cb83, $cb84, $cb85, $cb86, $cb87, $cb88, $cb89, $cb8a, $cb8b, $cb8c, $cb8d, $cb8e, $cb8f, $cb90, $cb91, $cb92, $cb93, $cb94, $cb95, $cb96, $cb97, $cb98, $cb99, $cb9a, $cb9b, $cb9c, $cb9d, $cb9e, $cb9f, $cba0, $cba1, $cba2, $cba3, $cba4, $cba5, $cba6, $cba7, $cba8, $cba9, $cbaa, $cbab, $cbac, $cbad, $cbae, $cbaf, $cbb0, $cbb1, $cbb2, $cbb3, $cbb4, $cbb5, $cbb6, $cbb7, $cbb8, $cbb9, $cbba, $cbbb, $cbbc, $cbbd, $cbbe, $cbbf:
      begin
        res_opcode(opcode);
      end;
    $cbc0, $cbc1, $cbc2, $cbc3, $cbc4, $cbc5, $cbc6, $cbc7, $cbc8, $cbc9, $cbca, $cbcb, $cbcc, $cbcd, $cbce, $cbcf, $cbd0, $cbd1, $cbd2, $cbd3, $cbd4, $cbd5, $cbd6, $cbd7, $cbd8, $cbd9, $cbda, $cbdb, $cbdc, $cbdd, $cbde, $cbdf, $cbe0, $cbe1, $cbe2, $cbe3, $cbe4, $cbe5, $cbe6, $cbe7, $cbe8, $cbe9, $cbea, $cbeb, $cbec, $cbed, $cbee, $cbef, $cbf0, $cbf1, $cbf2, $cbf3, $cbf4, $cbf5, $cbf6, $cbf7, $cbf8, $cbf9, $cbfa, $cbfb, $cbfc, $cbfd, $cbfe, $cbff:
      begin
        set_opcode(opcode);
      end;
//cur
  else
    // 异常，没找到opcode，理论上不存在
  end;
end;

procedure TGBCpu.dec_opcode(opcode: Byte);
var
  value, oldvalue, address: Integer;
begin
  case opcode of
    $05:
      begin//DEC B
        oldvalue := getReg_B();
        setReg_B(oldvalue - 1);
        value := getReg_B();
//      consumeClockCycles(4);
      end;
    $0d:
      begin//DEC C
        oldvalue := getReg_C();
        setReg_C(oldvalue - 1);
        value := getReg_C();
//      consumeClockCycles(4);
      end;
    $15:
      begin//DEC D
        oldvalue := getReg_D();
        setReg_D(oldvalue - 1);
        value := getReg_D();
//      consumeClockCycles(4);
      end;
    $1d:
      begin//DEC E
        oldvalue := getReg_E();
        setReg_E(oldvalue - 1);
        value := getReg_E();
//      consumeClockCycles(4);
      end;
    $25:
      begin//INC H
        oldvalue := getReg_H();
        setReg_H(oldvalue - 1);
        value := getReg_H();
//      consumeClockCycles(4);
      end;
    $2d:
      begin//DEC L
        oldvalue := getReg_L();
        setReg_L(oldvalue - 1);
        value := getReg_L();
//      consumeClockCycles(4);
      end;
    $35:
      begin//DEC (HL)
        address := getReg_HL();
        value := _PGBMem^.ReadGBMemory(address);
        oldvalue := value;
        value := value - 1;
        value := value and 255;
        _PGBMem^.WriteGBMemory(address, value);
//      consumeClockCycles(12);
      end;
    $3d:
      begin//DEC A
        oldvalue := getReg_A();
        setReg_A(oldvalue - 1);
        value := getReg_A();
//      consumeClockCycles(4);
      end;
  end;
  // 处理标志位寄存器
  if (value = 0) then
  begin
    setFlag_Z(True);
  end
  else
  begin
    setFlag_Z(False);
  end;
  setFlag_N(True);
  if (oldvalue and $0F) < 1 then
//(val & 0x0F) == 0xF
//  if (value and $0F) = $0F then     //testcode wjl@2019/5/20
  begin
    setFlag_H(True);
  end
  else
  begin
    setFlag_H(False);
  end;
  if opcode = $35 then
    consumeClockCycles(12)
  else
    consumeClockCycles(4);
end;

function TGBCpu.getFlag_C: Boolean;
var
  tmp: Integer;
begin
  tmp := getReg_F shr 4; //取前4位
  tmp := tmp and $1;
  //左第1位取
  if tmp = 1 then
    result := True
  else
    result := False;
end;

function TGBCpu.getFlag_H: Boolean;
var
  tmp: Integer;
begin
  tmp := getReg_F shr 4; //取前4位
  tmp := tmp and $2;
  tmp := tmp shr 1; //左第2位取
  if tmp = 1 then
    result := True
  else
    result := False;
end;

function TGBCpu.getFlag_N: Boolean;
var
  tmp: Integer;
begin
  tmp := getReg_F shr 4; //取前4位
  tmp := tmp and $4;
  tmp := tmp shr 2; //左第2位取
  if tmp = 1 then
    result := True
  else
    result := False;
end;

function TGBCpu.getFlag_Z: Boolean;
var
  tmp: Integer;
begin
  tmp := getReg_F shr 4; //取前4位
  tmp := tmp and $8;
  tmp := tmp shr 3; //左第1位取
  if tmp = 1 then
    result := True
  else
    result := False;
end;

function TGBCpu.getOpcode: Integer;
var
  opcode: Integer;
begin
  opcode := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  // 如果是读取Prefix CB指令，则再读取一个字节，拼装成Prefit cb指令，2字节
  // 如 $CB7C
  // bug fix ，如果是$CB指令，则原定义的Byte类型shl后会变0。
  if opcode = $cb then
  begin
    opcode := opcode shl 8;
    opcode := opcode or _PGBMem^.ReadGBMemory(Reg_PC);
    Inc(Reg_PC);
  end;
  result := opcode;
end;

function TGBCpu.getReg_A: Byte;
begin
  result := Reg_A;
end;

function TGBCpu.getReg_F: Byte;
begin
  result := Reg_F;
end;

function TGBCpu.getReg_AF: Word;
begin
  result := Reg_A * $100 + Reg_F;
end;

function TGBCpu.getReg_B: Byte;
begin
  result := Reg_B;
end;

function TGBCpu.getReg_C: Byte;
begin
  result := Reg_C;
end;

function TGBCpu.getReg_BC: Word;
begin
  result := Reg_B * $100 + Reg_C;
end;

function TGBCpu.getReg_D: Byte;
begin
  result := Reg_D;
end;

function TGBCpu.getReg_E: Byte;
begin
  result := Reg_E;
end;

function TGBCpu.getReg_DE: Word;
begin
  result := Reg_D * $100 + Reg_E;
end;

function TGBCpu.getReg_H: Byte;
begin
  result := Reg_H;
end;

function TGBCpu.getReg_L: Byte;
begin
  result := Reg_L;
end;

function TGBCpu.getReg_SP_High: Byte;
begin
  result := Reg_SP shr 8;
end;

function TGBCpu.getReg_SP_Low: Byte;
begin
  result := Reg_SP;
end;

procedure TGBCpu.inc16_opcode(opcode: Byte);
begin

end;

procedure TGBCpu.inc_opcode(opcode: Byte);
var
  address, value: Integer;
begin
  case opcode of
    $3C:
      begin
        setReg_A(getReg_A + 1);
        value := getReg_A;
//      consumeClockCycles(4);
      end;
    $04:
      begin
        setReg_B(getReg_B + 1);
        value := getReg_B;
//      consumeClockCycles(4);
      end;
    $0C:
      begin
        setReg_C(getReg_C + 1);
        value := getReg_C;
//      consumeClockCycles(4);
      end;
    $14:
      begin
        setReg_D(getReg_D + 1);
        value := getReg_D;
//      consumeClockCycles(4);
      end;
    $1C:
      begin
        setReg_E(getReg_E + 1);
        value := getReg_E;
//      consumeClockCycles(4);
      end;
    $24:
      begin
        setReg_H(getReg_H + 1);
        value := getReg_H;
//      consumeClockCycles(4);
      end;
    $2C:
      begin
        setReg_L(getReg_L + 1);
        value := getReg_L;
//      consumeClockCycles(4);
      end;
    $34:
      begin
        address := getReg_HL;
        value := _pGBMem^.ReadGBMemory(address) + 1;
        _pGBMem^.WriteGBMemory(address, (value and 255));
//      consumeClockCycles(12);
      end;
  end;
  if (value and 255) = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  if (((value - 1) and $F) + ($F and 1)) > $F then
    setFlag_H(True)
  else
    setFlag_H(False);
  if (opcode = $34) then
    consumeClockCycles(12)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.jmp_opcode(opcode: Byte);
var
  address, temp: Integer;
begin
  address := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  temp := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  temp := temp shl 8;
  address := address or temp;
  Reg_PC := address;
  consumeClockCycles(16);
end;

procedure TGBCpu.jpcc_opcpode(opcode: Byte);
var
  condition: Boolean;
  address, temp: Integer;
begin
  condition := False;
  case opcode of
    $C2:
      begin
        condition := not getFlag_Z;
      end;
    $CA:
      begin
        condition := getFlag_Z;
      end;
    $D2:
      begin
        condition := not getFlag_C;
      end;
    $DA:
      begin
        condition := getFlag_C;
      end;
  end;
  address := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  temp := _PGBMem^.ReadGBMemory(Reg_PC);
  Inc(Reg_PC);
  if not condition then
  begin
    consumeClockCycles(12);
  end
  else
  begin
    temp := temp shl 8;
    address := address or temp;
    Reg_PC := address;
    consumeClockCycles(16);
  end;
end;

procedure TGBCpu.jrcc_opcode(opcode: Byte);
var
  condition: Boolean;
  n: Integer;
begin
  condition := False;
  case opcode of
    $20:
      begin//JR NZ,r8
        condition := not getFlag_Z;
      end;
    $28:
      begin//JR Z,r8
        condition := getFlag_Z;
      end;
    $30:
      begin//JR NC,r8
        condition := not getFlag_C;
      end;
    $38:
      begin//JR C,r8
        condition := getFlag_C
      end;
  end;
  n := _PGBMem^.ReadGBMemory(Reg_PC);
  Reg_PC := Reg_PC + 1;
  if (not condition) then
  begin
    consumeClockCycles(8);
    exit;
  end;
  if (n > 127) then
  begin
    n := -((not n + 1) and 255);
  end;
  Reg_PC := Reg_PC + n;
  consumeClockCycles(12);
end;

procedure TGBCpu.load_opcode(opcode: Byte);
var
  temp, result, low, up, address: Integer;
begin
  case opcode of
    //LD nn, n
    $06:
      begin
        setReg_B(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    $0E:
      begin
        setReg_C(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    $16:
      begin
        setReg_D(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    $1E:
      begin
        setReg_E(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    $26:
      begin
        setReg_H(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    $2E:
      begin
        setReg_L(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    //LD r1,r2
      $78:
      begin
        setReg_A(getReg_B);
        consumeClockCycles(4);
      end;
    $7F:
      begin
        setReg_A(getReg_A);
        consumeClockCycles(4);
      end;
    $79:
      begin
        setReg_A(getReg_C);
        consumeClockCycles(4);
      end;
    $7A:
      begin
        setReg_A(getReg_D);
        consumeClockCycles(4);
      end;
    $7B:
      begin
        setReg_A(getReg_E);
        consumeClockCycles(4);
      end;
    $7C:
      begin
        setReg_A(getReg_H);
        consumeClockCycles(4);
      end;
    $7D:
      begin
        setReg_A(getReg_L);
        consumeClockCycles(4);
      end;
    $7E:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $40:
      begin
        setReg_B(getReg_B);
        consumeClockCycles(4);
      end;
    $41:
      begin
        setReg_B(getReg_C);
        consumeClockCycles(4);
      end;
    $42:
      begin
        setReg_B(getReg_D);
        consumeClockCycles(4);
      end;
    $43:
      begin
        setReg_B(getReg_E);
        consumeClockCycles(4);
      end;
    $44:
      begin
        setReg_B(getReg_H);
        consumeClockCycles(4);
      end;
    $45:
      begin
        setReg_B(getReg_L);
        consumeClockCycles(4);
      end;
    $46:
      begin
        setReg_B(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $48:
      begin
        setReg_C(getReg_B);
        consumeClockCycles(4);
      end;
    $49:
      begin
        setReg_C(getReg_C);
        consumeClockCycles(4);
      end;
    $4A:
      begin
        setReg_C(getReg_D);
        consumeClockCycles(4);
      end;
    $4B:
      begin
        setReg_C(getReg_E);
        consumeClockCycles(4);
      end;
    $4C:
      begin
        setReg_C(getReg_H);
        consumeClockCycles(4);
      end;
    $4D:
      begin
        setReg_C(getReg_L);
        consumeClockCycles(4);
      end;
    $4E:
      begin
        setReg_C(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $50:
      begin
        setReg_D(getReg_B);
        consumeClockCycles(4);
      end;
    $51:
      begin
        setReg_D(getReg_C);
        consumeClockCycles(4);
      end;
    $52:
      begin
        setReg_D(getReg_D);
        consumeClockCycles(4);
      end;
    $53:
      begin
        setReg_D(getReg_E);
        consumeClockCycles(4);
      end;
    $54:
      begin
        setReg_D(getReg_H);
        consumeClockCycles(4);
      end;
    $55:
      begin
        setReg_D(getReg_L);
        consumeClockCycles(4);
      end;
    $56:
      begin
        setReg_D(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $58:
      begin
        setReg_E(getReg_B);
        consumeClockCycles(4);
      end;
    $59:
      begin
        setReg_E(getReg_C);
        consumeClockCycles(4);
      end;
    $5A:
      begin
        setReg_E(getReg_D);
        consumeClockCycles(4);
      end;
    $5B:
      begin
        setReg_E(getReg_E);
        consumeClockCycles(4);
      end;
    $5C:
      begin
        setReg_E(getReg_H);
        consumeClockCycles(4);
      end;
    $5D:
      begin
        setReg_E(getReg_L);
        consumeClockCycles(4);
      end;
    $5E:
      begin
        setReg_E(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $60:
      begin
        setReg_H(getReg_B);
        consumeClockCycles(4);
      end;
    $61:
      begin
        setReg_H(getReg_C);
        consumeClockCycles(4);
      end;
    $62:
      begin
        setReg_H(getReg_D);
        consumeClockCycles(4);
      end;
    $63:
      begin
        setReg_H(getReg_E);
        consumeClockCycles(4);
      end;
    $64:
      begin
        setReg_H(getReg_H);
        consumeClockCycles(4);
      end;
    $65:
      begin
        setReg_H(getReg_L);
        consumeClockCycles(4);
      end;
    $66:
      begin
        setReg_H(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $68:
      begin
        setReg_L(getReg_B);
        consumeClockCycles(4);
      end;
    $69:
      begin
        setReg_L(getReg_C);
        consumeClockCycles(4);
      end;
    $6A:
      begin
        setReg_L(getReg_D);
        consumeClockCycles(4);
      end;
    $6B:
      begin
        setReg_L(getReg_E);
        consumeClockCycles(4);
      end;
    $6C:
      begin
        setReg_L(getReg_H);
        consumeClockCycles(4);
      end;
    $6D:
      begin
        setReg_L(getReg_L);
        consumeClockCycles(4);
      end;
    $6E:
      begin
        setReg_L(_PGBMem^.ReadGBMemory(getReg_HL));
        consumeClockCycles(8);
      end;
    $70:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_B);
        consumeClockCycles(8);
      end;
    $71:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_C);
        consumeClockCycles(8);
      end;
    $72:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_D);
        consumeClockCycles(8);
      end;
    $73:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_E);
        consumeClockCycles(8);
      end;
    $74:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_H);
        consumeClockCycles(8);
      end;
    $75:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_L);
        consumeClockCycles(8);
      end;
    $36:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, _PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    //LD A,n
      $0A:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(getReg_BC));
        consumeClockCycles(8);
      end;
    $1A:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(getReg_DE));
        consumeClockCycles(8);
      end;
    $FA:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(_PGBMem^.ReadGBMemoryWord(Reg_PC)));
        Inc(Reg_PC);
        Inc(Reg_PC);
        consumeClockCycles(16);
      end;
    $3E:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(8);
      end;
    //LD n,A
      $47:
      begin
        setReg_B(getReg_A);
        consumeClockCycles(4);
      end;
    $4F:
      begin
        setReg_C(getReg_A);
        consumeClockCycles(4);
      end;
    $57:
      begin
        setReg_D(getReg_A);
        consumeClockCycles(4);
      end;
    $5F:
      begin
        setReg_E(getReg_A);
        consumeClockCycles(4);
      end;
    $67:
      begin
        setReg_H(getReg_A);
        consumeClockCycles(4);
      end;
    $6F:
      begin
        setReg_L(getReg_A);
        consumeClockCycles(4);
      end;
    $02:
      begin
        _PGBMem^.WriteGBMemory(getReg_BC, getReg_A);
        consumeClockCycles(8);
      end;
    $12:
      begin
        _PGBMem^.WriteGBMemory(getReg_DE, getReg_A);
        consumeClockCycles(8);
      end;
    $77:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_A);
        consumeClockCycles(8);
      end;
    $EA:
      begin
        _PGBMem^.WriteGBMemory(_PGBMem^.ReadGBMemoryWord(Reg_PC), getReg_A);
        Inc(Reg_PC);
        Inc(Reg_PC);
        consumeClockCycles(16);
      end;
    $F2:
      begin
      // bug fix，读寄存器C，这里写成了PC....
        setReg_A(_PGBMem^.ReadGBMemory(getReg_C + $FF00));
        consumeClockCycles(8);
      end;
    $E2:
      begin
        _PGBMem^.WriteGBMemory($FF00 + getReg_C, getReg_A);
        consumeClockCycles(8);
      end;
    $3A:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(getReg_HL));
        setReg_HL(getReg_HL - 1);
        consumeClockCycles(8);
      end;
    $32:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_A);
        setReg_HL(getReg_HL - 1);
        consumeClockCycles(8);
      end;
    $2A:
      begin
        setReg_A(_PGBMem^.ReadGBMemory(getReg_HL));
        setReg_HL(getReg_HL + 1);
        consumeClockCycles(8);
      end;
    $22:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, getReg_A);
        setReg_HL(getReg_HL + 1);
        consumeClockCycles(8);
      end;
    $E0:
      begin
        _PGBMem^.WriteGBMemory($FF00 + _PGBMem^.ReadGBMemory(Reg_PC), getReg_A);
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $F0:
      begin
        setReg_A(_PGBMem^.ReadGBMemory($FF00 + _PGBMem^.ReadGBMemory(Reg_PC)));
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $01:
      begin//LD BC,nn
        setReg_C(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        setReg_B(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $11:
      begin
    // bug fix ，E和D的位置搞反了
        setReg_E(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        setReg_D(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $21:
      begin
    // bug fix ，抄的时候抄错了，先写L再写H
        setReg_L(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        setReg_H(_PGBMem^.ReadGBMemory(Reg_PC));
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $31:
      begin
        Reg_SP := _PGBMem^.ReadGBMemoryWord(Reg_PC);
        Inc(Reg_PC);
        Inc(Reg_PC);
        consumeClockCycles(12);
      end;
    $F9:
      begin
        Reg_SP := getReg_HL;
        consumeClockCycles(8);
      end;
    $F8:
      begin
        temp := _PGBMem^.ReadGBMemory(Reg_PC);
        if (temp > 127) then
        begin
          temp := -((not temp + 1) and 255);
        end;
        Inc(Reg_PC);
        result := temp + Reg_SP;
        setReg_H((result shr 8) and 255);
        setReg_L(result and 255);
        setFlag_Z(False);
        setFLag_N(False);
        if ((Reg_SP xor temp xor result) and $100) = $100 then
        begin
          setFlag_C(True);
        end
        else
        begin
          setFlag_C(False);
        end;
        if ((Reg_SP xor temp xor result) and $10) = $10 then
        begin
          setFlag_H(True);
        end
        else
        begin
          setFlag_H(False);
        end;
        consumeClockCycles(12);
      end;
    $08:
      begin
        low := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
        up := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
        address := ((up shl 8) + low);
        _PGBMem^.WriteGBMemory(address, getReg_SP_Low);
        _PGBMem^.WriteGBMemory(address + 1, getReg_SP_High); // fix 本身就是byte，不用再shr8
        consumeClockCycles(20);
      end;
  end;
end;

procedure TGBCpu.main;
var
  I: Integer;
begin
//  while not Paused do
//  begin
  _instr_count := 0;
//  skipBios;
  while not Paused do
  begin
    if _instr_count > 300000 then
    begin
      _debug_log_list.SaveToFile('d:\wjlgb.txt');
      _instr_count := 300000;
    end;

    step(1);
  end;
  //Form1.Memo1.Lines.Add(_debug_log);
end;

procedure TGBCpu.or_opcode(opcode: Byte);
var
  second: Integer;
begin
  second := 0;
  case opcode of
    $B7:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $B0:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $B1:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $B2:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $B3:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $B4:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $B5:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $B6:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $F6:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  setReg_A(getReg_A or second);
  if getReg_A = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(False);
  setFlag_C(False);
  if (opcode = $B6) or (opcode = $F6) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

function TGBCpu.popHelper: Integer;
var
  low, high: Integer;
begin
  low := _PGBMem^.ReadGBMemory(Reg_SP);
  Reg_SP := Reg_SP + 1;
  high := _PGBMem^.ReadGBMemory(Reg_SP);
  Reg_SP := Reg_SP + 1;
  high := high shl 8;
  result := high or low;
end;

procedure TGBCpu.pop_opcode(opcode: Integer);
begin
  case opcode of
    $F1:
      begin//POP AF
        setReg_AF(popHelper);
        consumeClockCycles(12);
      end;
    $C1:
      begin//POP BC
        setReg_BC(popHelper);
        consumeClockCycles(12);
      end;
    $D1:
      begin//POP DE
        setReg_DE(popHelper);
        consumeClockCycles(12);
      end;
    $E1:
      begin//POP HL
        setReg_HL(popHelper);
        consumeClockCycles(12);
      end;
  end;
end;

procedure TGBCpu.processEi(opcode: Integer);
begin
  if pendingInterruptEnable and (opcode <> $FB) then
  begin
    pendingInterruptEnable := False;
    GBInterrupt.Instance.masterEnable;
  end;
end;

procedure TGBCpu.processInterrupts;
var
  _gbInterrupt: TGBInterrupt;
  I: Integer;
  isAnyInterruptGettingHandled: Boolean;
begin
  isAnyInterruptGettingHandled := False;
  for I := 0 to 4 do
  begin
    _gbInterrupt := GBInterrupt.Instance.getAllInterrupt[I];
    if _gbInterrupt.isRaised then
    begin
      isHalted := False;
      if GBInterrupt.Instance.isMasterEnabled then
      begin
        if _gbInterrupt.isEnabled then
        begin
          pushHelper(Reg_PC);
          Reg_PC := _gbInterrupt.handler;
          GBInterrupt.Instance.clearInterruptByIdx(I);
          GBInterrupt.Instance.masterDisable;
//          consumeClockCycles(20);    // 是否真需要执行20周期？？？
        end;
      end;
    end;
  end;
end;

procedure TGBCpu.pushHelper(value: Integer);
begin
  Reg_SP := Reg_SP - 1;
  _PGBMem^.WriteGBMemory(Reg_SP, (value and $FF00) shr 8);
  Reg_SP := Reg_SP - 1;
  _PGBMem^.WriteGBMemory(Reg_SP, value and $00FF);
end;

procedure TGBCpu.push_opcode(opcode: Integer);
begin
  case opcode of
    $F5:
      begin//PUSH AF
        pushHelper(getReg_AF);
        consumeClockCycles(16);
      end;
    $C5:
      begin//PUSH BC
        pushHelper(getReg_BC);
        consumeClockCycles(16);
      end;
    $D5:
      begin//PUSH DE
        pushHelper(getReg_DE);
        consumeClockCycles(16);
      end;
    $E5:
      begin//PUSH HL
        pushHelper(getReg_HL);
        consumeClockCycles(16);
      end;
  end;
end;

procedure TGBCpu.rrc_opcode(opcode: Word);
var
  value, result, oldbit0: Integer;
begin
  case opcode of
    $CB0F:
      begin
        value := getReg_A;
      end;
    $CB08:
      begin
        value := getReg_B;
      end;
    $CB09:
      begin
        value := getReg_C;
      end;
    $CB0A:
      begin
        value := getReg_D;
      end;
    $CB0B:
      begin
        value := getReg_E;
      end;
    $CB0C:
      begin
        value := getReg_H;
      end;
    $CB0D:
      begin
        value := getReg_L;
      end;
    $CB0E:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  oldbit0 := value and $01;
  value := value shr 1;
  result := value or (oldbit0 shl 7);
  case opcode of
    $CB0F:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB08:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB09:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB0A:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB0B:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB0C:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB0D:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB0E:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(False);
  if oldbit0 = 1 then
    setFlag_C(True)
  else
    setFlag_C(False);
  if (opcode = $CB0E) then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.res_opcode(opcode: Word);
var
  value, mask, bitIndex: Integer;
begin
  value := cbHelperRead(opcode);
  bitIndex := (opcode - $CB80) div 8;
  mask := 1 shl bitIndex;
  mask := not mask;
  value := value and mask;
  cbHelperWrite(opcode, value);
end;

procedure TGBCpu.retcc_opcode(opcode: Byte);
var
  condition: Boolean;
begin
  case opcode of
    $C0:
      begin
        condition := not getFlag_Z;
      end;
    $C8:
      begin
        condition := getFlag_Z;
      end;
    $D0:
      begin
        condition := not getFlag_C;
      end;
    $D8:
      begin
        condition := getFlag_C;
      end;
  end;
  if not condition then
  begin
    consumeClockCycles(8);
  end
  else
  begin
    retHelper();
    consumeClockCycles(20);
  end;
end;

procedure TGBCpu.retHelper;
begin
  Reg_PC := popHelper();
end;

procedure TGBCpu.rlc_opcode(opcode: Word);
var
  value, result: Integer;
  bit7: Boolean;
begin
  case opcode of
    $CB07:
      begin
        value := getReg_A;
      end;
    $CB00:
      begin
        value := getReg_B;
      end;
    $CB01:
      begin
        value := getReg_C;
      end;
    $CB02:
      begin
        value := getReg_D;
      end;
    $CB03:
      begin
        value := getReg_E;
      end;
    $CB04:
      begin
        value := getReg_H;
      end;
    $CB05:
      begin
        value := getReg_L;
      end;
    $CB06:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  // bug fix ,CB系，错一堆了。
  result := (value shl 1) or (value shr 7);
  if ((value and $80) shr 7) = 1 then
    bit7 := True
  else
    bit7 := False;
  result := result and 255;
  case opcode of
    $CB07:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB00:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB01:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB02:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB03:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB04:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB05:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB06:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  setFlag_C(bit7);
  setFlag_H(False);
  setFlag_N(False);
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if opcode = $CB06 then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.rl_opcode(opcode: Word);
var
  value, result: Integer;
  bit7: Boolean;
begin
  case opcode of
    $CB17:
      begin
        value := getReg_A;
      end;
    $CB10:
      begin
        value := getReg_B;
      end;
    $CB11:
      begin
        value := getReg_C;
      end;
    $CB12:
      begin
        value := getReg_D;
      end;
    $CB13:
      begin
        value := getReg_E;
      end;
    $CB14:
      begin
        value := getReg_H;
      end;
    $CB15:
      begin
        value := getReg_L;
      end;
    $CB16:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  if (value and $80) > 0 then
    bit7 := True
  else
    bit7 := False;
  result := (value shl 1) and $FF;
  if getFlag_C then
    result := result or 1
  else
    result := result or 0;
  case opcode of
    $CB17:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB10:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB11:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB12:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB13:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB14:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB15:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB16:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(False);
  setFlag_C(bit7);
  if opcode = $CB16 then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.rr_opcode(opcode: Word);
var
  value, result: Integer;
begin
  case opcode of
    $CB1F:
      begin
        value := getReg_A;
      end;
    $CB18:
      begin
        value := getReg_B;
      end;
    $CB19:
      begin
        value := getReg_C;
      end;
    $CB1A:
      begin
        value := getReg_D;
      end;
    $CB1B:
      begin
        value := getReg_E;
      end;
    $CB1C:
      begin
        value := getReg_H;
      end;
    $CB1D:
      begin
        value := getReg_L;
      end;
    $CB1E:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  if getFlag_C then
    result := (value shr 1) or (1 shl 7)
  else
    result := (value shr 1) or (0 shl 7);
  case opcode of
    $CB1F:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB18:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB19:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB1A:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB1B:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB1C:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB1D:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB1E:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  if (value and $1) = 1 then
    setFlag_C(True)
  else
    setFlag_C(False);
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(False);
  if opcode = $CB1E then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.rst_opcode(opcode: Byte);
begin
  pushHelper(Reg_PC);
  case opcode of
    $C7:
      begin
        Reg_PC := $00;
      end;
    $CF:
      begin
        Reg_PC := $08;
      end;
    $D7:
      begin
        Reg_PC := $10;
      end;
    $DF:
      begin
        Reg_PC := $18;
      end;
    $E7:
      begin
        Reg_PC := $20;
      end;
    $EF:
      begin
        Reg_PC := $28;
      end;
    $F7:
      begin
        Reg_PC := $30;
      end;
    $FF:
      begin
        Reg_PC := $38;
      end;
  end;
  consumeClockCycles(16);
end;

function TGBCpu.getReg_HL: Word;
begin
  result := Reg_H * $100 + Reg_L;
end;

procedure TGBCpu.sbc_opcode(opcode: Byte);
var
  second, result: Integer;
begin
  case opcode of
    $9F:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $98:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $99:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $9A:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $9B:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $9C:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $9D:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $9E:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $DE:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  result := getReg_A - second;
  if getFlag_C then
    result := result - 1;
  setFlag_N(True);
  if result and 255 = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if result and $100 <> 0 then
    setFlag_C(True)
  else
    setFlag_C(False);
  if ((getReg_A xor second xor result) and $10) <> 0 then
    setFlag_H(True)
  else
    setFlag_H(False);
  if (result > 255) or (result < 0) then
    result := result and 255;
  setReg_A(result);
  if (opcode = $9E) or (opcode = $DE) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.setFlag_C(value: Boolean);
begin
  if value then
    setReg_F(getReg_F or $10)
  else
    setReg_F(getReg_F and $EF);
end;

procedure TGBCpu.setFlag_H(value: Boolean);
begin
  if value then
    setReg_F(getReg_F or $20)
  else
    setReg_F(getReg_F and $DF);
end;

procedure TGBCpu.setFlag_N(value: Boolean);
begin
  if value then
    setReg_F(getReg_F or $40)
  else
    setReg_F(getReg_F and $BF);
end;

procedure TGBCpu.setFlag_Z(value: Boolean);
begin
  if value then
    setReg_F(getReg_F or $80)
  else
    setReg_F(getReg_F and $7F);
end;

procedure TGBCpu.setReg_A(val: Byte);
begin
  Reg_A := val;
end;

procedure TGBCpu.setReg_F(val: Byte);
begin
  Reg_F := val;
end;

procedure TGBCpu.setReg_AF(val: Word);
var
  tmp: Byte;
begin       // add =
  if (val > $FFFF) or (val < 0) then
  begin
    val := val and $FFFF;
  end;
  Reg_A := val div 256;
  tmp := val; // 取Word的低4位变成Byte
  tmp := tmp and $F0; //将低4位变0，F寄存器低4位永远是0
  Reg_F := tmp;
end;

procedure TGBCpu.setReg_B(val: Byte);
begin
  Reg_B := val;
end;

procedure TGBCpu.setReg_C(val: Byte);
begin
  Reg_C := val;
end;

procedure TGBCpu.setReg_BC(val: Word);
begin
  Reg_B := val shr 8;
  Reg_C := val;
end;

procedure TGBCpu.setReg_D(val: Byte);
begin
  Reg_D := val;
end;

procedure TGBCpu.setReg_E(val: Byte);
begin
  Reg_E := val;
end;

procedure TGBCpu.setReg_DE(val: Word);
begin
  Reg_D := val shr 8;
  Reg_E := val;
end;

procedure TGBCpu.setReg_H(val: Byte);
begin
  Reg_H := val;
end;

procedure TGBCpu.setReg_L(val: Byte);
begin
  Reg_L := val;
end;

procedure TGBCpu.set_opcode(opcode: Word);
var
  value, bitIndex, mask: Integer;
begin
  value := cbHelperRead(opcode);
  bitIndex := (opcode - $CBC0) div 8;
  mask := 1 shl bitIndex;
  value := value or mask;
  cbHelperWrite(opcode, value);
end;

procedure TGBCpu.skipBios;
begin
  setReg_A($01);
  setReg_B($00);
  setReg_C($13);
  setReg_D($00);
  setReg_E($D8);
  setReg_H($01);
  setReg_L($4D);
  setFlag_Z(True);
  setFlag_N(False);
  setFlag_H(True);
  setFlag_C(True);
  Reg_SP := $FFFE;
  Reg_PC := $100;
  GBTimer.Instance.setDivBypass($AB);
  _pGBMem^.WriteGBMemory($FF0F, $E1);
  _pGBMem^.WriteGBMemory($FF05, $00); // TIMA
  _pGBMem^.WriteGBMemory($FF06, $00); // TMA
  _pGBMem^.WriteGBMemory($FF07, $00); // TAC
  _pGBMem^.WriteGBMemory($FF10, $80); // NR10
  _pGBMem^.WriteGBMemory($FF11, $BF); // NR11
  _pGBMem^.WriteGBMemory($FF12, $F3); // NR12
  _pGBMem^.WriteGBMemory($FF14, $BF); // NR14
  _pGBMem^.WriteGBMemory($FF16, $3F); // NR21
  _pGBMem^.WriteGBMemory($FF17, $00); // NR22
  _pGBMem^.WriteGBMemory($FF19, $BF); // NR24
  _pGBMem^.WriteGBMemory($FF1A, $7F); // NR30
  _pGBMem^.WriteGBMemory($FF1B, $FF); // NR31
  _pGBMem^.WriteGBMemory($FF1C, $9F); // NR32
  _pGBMem^.WriteGBMemory($FF1E, $BF); // NR33
  _pGBMem^.WriteGBMemory($FF20, $FF); // NR41
  _pGBMem^.WriteGBMemory($FF21, $00); // NR42
  _pGBMem^.WriteGBMemory($FF22, $00); // NR43
  _pGBMem^.WriteGBMemory($FF23, $BF); // NR30
  _pGBMem^.WriteGBMemory($FF24, $77); // NR50
  _pGBMem^.WriteGBMemory($FF25, $F3); // NR51
  _pGBMem^.WriteGBMemory($FF26, $F1); // NR52
  _pGBMem^.WriteGBMemory($FF40, $91); // LCDC
  _pGBMem^.WriteGBMemory($FF42, $00); // SCY
  _pGBMem^.WriteGBMemory($FF43, $00); // SCX
  _pGBMem^.WriteGBMemory($FF45, $00); // LYC
  _pGBMem^.WriteGBMemory($FF47, $FC); // BGP
  _pGBMem^.WriteGBMemory($FF48, $FF); // OBP0
  _pGBMem^.WriteGBMemory($FF49, $FF); // OBP1
  _pGBMem^.WriteGBMemory($FF4A, $00); // WY
  _pGBMem^.WriteGBMemory($FF4B, $00); // WX
  _pGBMem^.WriteGBMemory($FFFF, $00); // IE
end;

procedure TGBCpu.sla_opcode(opcode: Word);
var
  value, result: Integer;
  carry: Boolean;
begin
  case opcode of
    $CB27:
      begin
        value := getReg_A;
      end;
    $CB20:
      begin
        value := getReg_B;
      end;
    $CB21:
      begin
        value := getReg_C;
      end;
    $CB22:
      begin
        value := getReg_D;
      end;
    $CB23:
      begin
        value := getReg_E;
      end;
    $CB24:
      begin
        value := getReg_H;
      end;
    $CB25:
      begin
        value := getReg_L;
      end;
    $CB26:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  if ((value and $80) shr 7) = 1 then
    carry := True
  else
    carry := False;
  result := value shl 1;
  result := result and 255;
  case opcode of
    $CB27:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB20:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB21:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB22:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB23:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB24:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB25:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB26:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_C(carry);
  setFlag_N(False);
  setFlag_H(False);
  if opcode = $CB26 then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.sra_opcode(opcode: Word);
var
  value, result: Integer;
  carry: Boolean;
begin
  case opcode of
    $CB2F:
      begin
        value := getReg_A;
      end;
    $CB28:
      begin
        value := getReg_B;
      end;
    $CB29:
      begin
        value := getReg_C;
      end;
    $CB2A:
      begin
        value := getReg_D;
      end;
    $CB2B:
      begin
        value := getReg_E;
      end;
    $CB2C:
      begin
        value := getReg_H;
      end;
    $CB2D:
      begin
        value := getReg_L;
      end;
    $CB2E:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  // bug fix again
  if (value and 1) <> 0 then
    carry := True
  else
    carry := False;
  result := (value shr 1) or (value and $80);
  case opcode of
    $CB2F:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB28:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB29:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB2A:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB2B:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB2C:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB2D:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB2E:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  setFlag_C(carry);
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_H(False);
  setFlag_N(False);
  if opcode = $CB2E then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.srl_opcode(opcode: Word);
var
  value, result: Integer;
  carry: Boolean;
begin
  case opcode of
    $CB3F:
      begin
        value := getReg_A;
      end;
    $CB38:
      begin
        value := getReg_B;
      end;
    $CB39:
      begin
        value := getReg_C;
      end;
    $CB3A:
      begin
        value := getReg_D;
      end;
    $CB3B:
      begin
        value := getReg_E;
      end;
    $CB3C:
      begin
        value := getReg_H;
      end;
    $CB3D:
      begin
        value := getReg_L;
      end;
    $CB3E:
      begin
        value := _PGBMem^.ReadGBMemory(getReg_HL);
      end;
  end;
  if value and $1 <> 0 then
    carry := True
  else
    carry := False;
  result := value shr 1;
  case opcode of
    $CB3F:
      begin
        setReg_A(result);
//      consumeClockCycles(8);
      end;
    $CB38:
      begin
        setReg_B(result);
//      consumeClockCycles(8);
      end;
    $CB39:
      begin
        setReg_C(result);
//      consumeClockCycles(8);
      end;
    $CB3A:
      begin
        setReg_D(result);
//      consumeClockCycles(8);
      end;
    $CB3B:
      begin
        setReg_E(result);
//      consumeClockCycles(8);
      end;
    $CB3C:
      begin
        setReg_H(result);
//      consumeClockCycles(8);
      end;
    $CB3D:
      begin
        setReg_L(result);
//      consumeClockCycles(8);
      end;
    $CB3E:
      begin
        _PGBMem^.WriteGBMemory(getReg_HL, result);
//      consumeClockCycles(16);
      end;
  end;
  if result = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_C(carry);
  setFlag_N(False);
  setFlag_H(False);
  if opcode = $CB3E then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.step(i: Integer);
var
  opcode: Integer;
begin
  opcode := 0;

  processInterrupts;
  if not isHalted then
  begin
    Inc(_icycle_count); //
    Inc(_instr_count);
//      if _instr_count = 6 then
//        _instr_count := 6;
    if _icycle_count = 75345 then
      _icycle_count := 75345;
    opcode := getOpcode;
    _debug_opcode := opcode;
//      if (_instr_count >=0) and (_instr_count < 300000) then
//      begin
//        _debug_log_list.Append(IntToStr(_instr_count)+' Opcode:'+IntToStr(opcode));
//        _debug_log_list.Append('REG_A:'+IntToStr(getReg_A));
//        _debug_log_list.Append('REG_B:'+IntToStr(getReg_B));
//        _debug_log_list.Append('REG_C:'+IntToStr(getReg_C));
//        _debug_log_list.Append('REG_D:'+IntToStr(getReg_D));
//        _debug_log_list.Append('REG_E:'+IntToStr(getReg_E));
//        _debug_log_list.Append('REG_H:'+IntToStr(getReg_H));
//        _debug_log_list.Append('REG_L:'+IntToStr(getReg_L));
//        _debug_log_list.Append('REG_F:'+IntToStr(Reg_F));
//        _debug_log_list.Append('REG_PC:'+IntToStr(Reg_PC));
//        _debug_log_list.Append('REG_SP:'+IntToStr(Reg_SP));
////          _debug_log_list.Append('GPU_Y:'+IntToStr(_pGBGpu^.scrollY));
////          _debug_log_list.Append('GPU_X:'+IntToStr(_pGBGpu^.scrollX));
////        _debug_log_list.Append('romBank:'+IntToStr(_PGBMem^.getMBCRomBank));
////          _debug_log_list.Append('GPULINE:'+IntToStr(_pGBGpu^.line));
////          _debug_log_list.Append('modeClock:'+IntToStr(_pGBGpu^.modeClock));
////        _debug_log_list.Append('WRAM_7425:'+IntToStr(_PGBMem^.ReadGBMemory(56577)));
//        if GBInterrupt.Instance.getAllInterrupt[3].isRaised then
//          _debug_log_list.Append('INT 3:isRaised=True')
//        else
//          _debug_log_list.Append('INT 3:isRaised=False');
//        if GBInterrupt.Instance.getAllInterrupt[3].isEnabled then
//          _debug_log_list.Append('INT 3:isEnabled=True')
//        else
//          _debug_log_list.Append('INT 3:isEnabled=False');
//      end;

    deCode(opcode);
    processEi(opcode);
  end
  else
  begin
    consumeClockCycles(4);
  end;
end;

procedure TGBCpu.sub_opcode(opcode: Byte);
var
  second, result: Integer;
begin
  case opcode of
    $97:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $90:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $91:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $92:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $93:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $94:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $95:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $96:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $D6:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  result := getReg_A - second;
  setFlag_N(True);
  if result and 255 = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  if second > getReg_A then
    setFlag_C(True)
  else
    setFlag_C(False);
  if ((getReg_A xor second xor result) and $10) <> 0 then
    setFlag_H(True)
  else
    setFlag_H(False);
  if (result > 255) or (result < 0) then
    result := result and 255;
  setReg_A(result);
  if (opcode = $96) or (opcode = $D6) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

function TGBCpu.swapHelper(value: Integer): Integer;
var
  upper, lower, tmp: Integer;
begin
  upper := value and $F0;
  lower := value and $0F;
  lower := lower shl 4;
  upper := upper shr 4;
  tmp := 0;
  tmp := tmp or upper;
  tmp := tmp or lower;
  result := tmp;
end;

procedure TGBCpu.swap_opcode(opcode: Word);
var
  value: Integer;
begin
  case opcode of
    $CB37:
      begin
        value := swapHelper(getReg_A);
        setReg_A(value);
//      consumeClockCycles(8);
      end;
    $CB30:
      begin
        value := swapHelper(getReg_B);
        setReg_B(value);
//      consumeClockCycles(8);
      end;
    $CB31:
      begin
        value := swapHelper(getReg_C);
        setReg_C(value);
//      consumeClockCycles(8);
      end;
    $CB32:
      begin
        value := swapHelper(getReg_D);
        setReg_D(value);
//      consumeClockCycles(8);
      end;
    $CB33:
      begin
        value := swapHelper(getReg_E);
        setReg_E(value);
//      consumeClockCycles(8);
      end;
    $CB34:
      begin
        value := swapHelper(getReg_H);
        setReg_H(value);
//      consumeClockCycles(8);
      end;
    $CB35:
      begin
        value := swapHelper(getReg_L);
        setReg_L(value);
//      consumeClockCycles(8);
      end;
    $CB36:
      begin
        value := swapHelper(_PGBMem^.ReadGBMemory(getReg_HL));
        _PGBMem^.WriteGBMemory(getReg_HL, value);
//      consumeClockCycles(16);
      end;
  end;
  if value = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_H(False);
  setFlag_N(False);
  setFlag_C(False);
  if opcode = $CB36 then
    consumeClockCycles(16)
  else
    consumeClockCycles(8);
end;

procedure TGBCpu.xor_opcode(opcode: Byte);
var
  second: Integer;
begin
  case opcode of
    $AF:
      begin
        second := getReg_A;
//      consumeClockCycles(4);
      end;
    $A8:
      begin
        second := getReg_B;
//      consumeClockCycles(4);
      end;
    $A9:
      begin
        second := getReg_C;
//      consumeClockCycles(4);
      end;
    $AA:
      begin
        second := getReg_D;
//      consumeClockCycles(4);
      end;
    $AB:
      begin
        second := getReg_E;
//      consumeClockCycles(4);
      end;
    $AC:
      begin
        second := getReg_H;
//      consumeClockCycles(4);
      end;
    $AD:
      begin
        second := getReg_L;
//      consumeClockCycles(4);
      end;
    $AE:
      begin
        second := _PGBMem^.ReadGBMemory(getReg_HL);
//      consumeClockCycles(8);
      end;
    $EE:
      begin
        second := _PGBMem^.ReadGBMemory(Reg_PC);
        Inc(Reg_PC);
//      consumeClockCycles(8);
      end;
  end;
  setReg_A(getReg_A xor second);
  if getReg_A = 0 then
    setFlag_Z(True)
  else
    setFlag_Z(False);
  setFlag_N(False);
  setFlag_H(False);
  setFlag_C(False);

  if (opcode = $AE) or (opcode = $EE) then
    consumeClockCycles(8)
  else
    consumeClockCycles(4);
end;

procedure TGBCpu.setReg_HL(val: Word);
begin
  Reg_H := val shr 8;
  Reg_L := val;
end;

end.

