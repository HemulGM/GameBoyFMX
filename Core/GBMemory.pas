unit GBMemory;

	{ 设置GB的内存表，具体分布如下如列
    那么关于GB的内存，主要有2个，一个是读内存，一个是写内存，就那么简单
		General Memory Map
		  0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
		  4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
		  8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
		  A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
		  C000-CFFF   4KB Work RAM Bank 0 (WRAM)
		  D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
		  E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
		  FE00-FE9F   Sprite Attribute Table (OAM)
		  FEA0-FEFF   Not Usable
		  FF00-FF7F   I/O Ports
		  FF80-FFFE   High RAM (HRAM)
		  FFFF        Interrupt Enable Register
	 }
interface

uses
  System.SysUtils, GBRom, GBGpu, GBMbc, GBTimer, GBInterruptManager, GBJoypad;

const
  GBBios: array[0..255] of Integer = (
    $31, $FE, $FF, $AF, $21, $FF, $9F, $32, $CB, $7C, $20, $FB, $21, $26, $FF, $0E,
    $11, $3E, $80, $32, $E2, $0C, $3E, $F3, $E2, $32, $3E, $77, $77, $3E, $FC, $E0,
    $47, $11, $04, $01, $21, $10, $80, $1A, $CD, $95, $00, $CD, $96, $00, $13, $7B,
    $FE, $34, $20, $F3, $11, $D8, $00, $06, $08, $1A, $13, $22, $23, $05, $20, $F9,
    $3E, $19, $EA, $10, $99, $21, $2F, $99, $0E, $0C, $3D, $28, $08, $32, $0D, $20,
    $F9, $2E, $0F, $18, $F3, $67, $3E, $64, $57, $E0, $42, $3E, $91, $E0, $40, $04,
    $1E, $02, $0E, $0C, $F0, $44, $FE, $90, $20, $FA, $0D, $20, $F7, $1D, $20, $F2,
    $0E, $13, $24, $7C, $1E, $83, $FE, $62, $28, $06, $1E, $C1, $FE, $64, $20, $06,
    $7B, $E2, $0C, $3E, $87, $F2, $F0, $42, $90, $E0, $42, $15, $20, $D2, $05, $20,
    $4F, $16, $20, $18, $CB, $4F, $06, $04, $C5, $CB, $11, $17, $C1, $CB, $11, $17,
    $05, $20, $F5, $22, $23, $22, $23, $C9, $CE, $ED, $66, $66, $CC, $0D, $00, $0B,
    $03, $73, $00, $83, $00, $0C, $00, $0D, $00, $08, $11, $1F, $88, $89, $00, $0E,
    $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99, $BB, $BB, $67, $63, $6E, $0E, $EC, $CC,
    $DD, $DC, $99, $9F, $BB, $B9, $33, $3E, $3c, $42, $B9, $A5, $B9, $A5, $42, $3C,
    $21, $04, $01, $11, $A8, $00, $1A, $13, $BE, $20, $FE, $23, $7D, $FE, $34, $20,
    $F5, $06, $19, $78, $86, $23, $05, $20, $FB, $86, $20, $FE, $3E, $01, $E0, $50);

type
  TGBMemory = class
  private
    _gpu: PGBGpu;
    _gbtimer: TGBTimer;
    _gbjoypad: TGBJoypad;
    _GBInterrupt: TGBInterruptManager;
    function processUnusedBits(address, value: Integer): Integer;
  public
    // 0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
    ROMBank00: array[0..16383] of byte;
    // 4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
    ROMBank01NN: array[0..16383] of byte;
    // 8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
    VRAM: array[0..8191] of byte;
    // A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
    ExtRAM: array[0..8191] of byte;
    // C000-CFFF   4KB Work RAM Bank 0 (WRAM)
    WRAM0: array[0..4095] of byte;
    // D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
    WRAM1: array[0..4095] of byte;
    WRAM: array[0..8191] of Byte; // 使用合并？
    // E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
    ECHO: array[0..7679] of byte;
    // FE00-FE9F   Sprite Attribute Table (OAM)
    OAM: array[0..159] of byte;
    // FEA0-FEFF   Not Usable
    // 所以不用定义，未使用内存
    // FF00-FF7F   I/O Ports
    IOPort: array[0..126] of byte;
    // FF80-FFFE   High RAM (HRAM)
    HRAM: array[0..126] of byte;
    // 写内存，位置，地址
//    rom: TGBRom;
//    _prom:PGBRom;
    _pMbc: PGBMbc;
    isUseBios: Boolean;
    procedure WriteGBMemory(pos: Integer; val: Byte);
    procedure WriteGBMemoryWord(pos: Integer; val: Word);
    // 读内存，位置，返回具体的值
    function ReadGBMemory(pos: Integer): Byte;
    function ReadGBMemoryWord(pos: Integer): Word;
    function getMBCRomBank(): Integer;
    procedure InitGBMemory();
    constructor Create(pgbmbc: PGBMbc; pgbgpu: PGBGpu); overload;
  end;

type
  PGBMemory = ^TGBMemory;

implementation

{ TGBMemory }

constructor TGBMemory.Create(pgbmbc: pgbmbc; pgbgpu: pgbgpu);
begin
//  _prom := pgbrom;
  _gpu := pgbgpu;
  _pMbc := pgbmbc;
  isUseBios := True;
  InitGBMemory;
end;

function TGBMemory.getMBCRomBank: Integer;
begin
  Result := _pMbc^.romBankSelected;
end;

procedure TGBMemory.InitGBMemory;
var
  I: Integer;
begin
  // init all memory??
  for I := 0 to Length(ROMBank00) do
  begin
    ROMBank00[I] := $00;
  end;
  for I := 0 to Length(ROMBank01NN) do
  begin
    ROMBank01NN[I] := $00;
  end;
  for I := 0 to Length(VRAM) do
  begin
    VRAM[I] := $00;
  end;
  for I := 0 to Length(ExtRAM) do
  begin
    ExtRAM[I] := $00;
  end;
  for I := 0 to Length(WRAM0) do
  begin
    WRAM0[I] := $00;
  end;
  for I := 0 to Length(WRAM1) do
  begin
    WRAM1[I] := $00;
  end;
  for I := 0 to Length(ECHO) do
  begin
    ECHO[I] := $00;
  end;
  for I := 0 to Length(OAM) do
  begin
    OAM[I] := $00;
  end;
  for I := 0 to Length(IOPort) do
  begin
    IOPort[I] := $00;
  end;
  for I := 0 to Length(HRAM) do
  begin
    HRAM[I] := $00;
  end;
  for I := 0 to Length(WRAM) do
  begin
    WRAM[I] := $00;
  end;
end;

function TGBMemory.processUnusedBits(address, value: Integer): Integer;
var
  ret: Integer;
begin
  case address of
    $FF02:
      begin
        ret := value or $7E;
      end;
    $FF07:
      begin
        ret := value or $F8;
      end;
    $ff0f:
      begin
        ret := value or $E0;
      end;
    $ff41, $FF10:
      begin
        ret := value or $80;
      end;
    $FF1A:
      begin
        ret := value or $7F;
      end;
    $FF1C:
      begin
        ret := value or $9F;
      end;
    $FF20:
      begin
        ret := value or $C0;
      end;
    $FF23:
      begin
        ret := value or $3F;
      end;
    $FF26:
      begin
        ret := value or $70;
      end;
    $ff03, $ff08, $ff09, $ff0a, $ff0b, $ff0c, $ff0d, $ff0e, $ff15, $ff1f, $ff27, $ff28, $ff29:
      begin
        ret := value or $FF;
      end;
  else
    ret := value;
  end;
  if (address >= $ff4c) and (address <= $ff7f) then
    ret := value or $FF;
  Result := ret;
end;

function TGBMemory.ReadGBMemory(pos: Integer): Byte;
begin
  Result := $00; // 初始值
  if (pos <= $7fff) then
  begin
    if isUseBios then
    begin
      if pos < $100 then
      begin
        Result := GBBios[pos];
        Exit;
      end
      else if pos = $100 then
      begin
        isUseBios := False;
      end;
    end;
    // 需使用mbc管理卡带
    Result := _pMbc^.MbcRead(pos);
  end
  else if (pos >= $8000) and (pos <= $9fff) then
    Result := _gpu^.vram[pos - $8000]
  else if (pos >= $a000) and (pos <= $bfff) then
    Result := _pMbc^.MbcRead(pos)// 需要用mbc管理
  else if (pos >= $c000) and (pos <= $dfff) then
    Result := WRAM[pos - $c000]
  else if (pos >= $e000) and (pos <= $deff) then
    Result := WRAM[pos - $e000]
  else if (pos >= $fe00) and (pos <= $feff) then
    Result := OAM[pos - $fe00]
  else if (pos = $ff40) then
    Result := _gpu^.LcdControl_getLcdControl
  else if (pos = $FF41) then
    Result := processUnusedBits(pos, _gpu^.LcdStatus_getLcdStatus)
  else if (pos = $FF42) then
    Result := _gpu^.scrollY
  else if (pos = $FF43) then
    Result := _gpu^.scrollX
  else if (pos = $FF44) then
    Result := _gpu^.line
  else if (pos = $FF45) then
    Result := _gpu^.lyc
  else if (pos = $FF00) then
  begin
    // Joypad.getInstance().getKeysPressed();
    Result := _gbjoypad.Instance.getGBKeyPressed;
//    Result := 223;//
  end
  else if (pos >= $FF01) and (pos <= $FFFF) then
  begin
    if pos = $FF04 then
      Result := _gbtimer.Instance.getDivider
    else if pos = $FF05 then
      Result := _gbtimer.Instance.getCounter
    else if pos = $FF06 then
      Result := _gbtimer.Instance.getModulo
    else if pos = $FF07 then
      Result := processUnusedBits(pos, _gbtimer.Instance.getControl)
    else if pos = $FF0F then
      Result := processUnusedBits(pos, _GBInterrupt.Instance.getInterruptsRaised)
    else if pos = $FFFF then
      Result := processUnusedBits(pos, _GBInterrupt.Instance.getInterruptsEnabled)
    else if (pos >= $FF80) and (pos <= $FFFE) then
      Result := processUnusedBits(pos, HRAM[pos - $FF80])
    else if (pos >= $FF10) and (pos <= $FF3F) then
      Result := processUnusedBits(pos, IOPort[pos - $FF10]) // sound hardware
    else
      Result := processUnusedBits(pos, 0);
  end;
end;

function TGBMemory.ReadGBMemoryWord(pos: Integer): Word;
var
  value: Integer;
begin
  value := ReadGBMemory(pos + 1);
  value := value shl 8;
  value := value + ReadGBMemory(pos);
  Result := value;
end;

procedure TGBMemory.WriteGBMemory(pos: Integer; val: Byte);
var
  I: Integer;
begin
//重写
// mbc暂不支持，后续支持了再补
//        if (address >= 0 && address <= 0x7fff) {
//            cartMbc.mbcWrite(address, value);
//        }
//        else if(address >= 0xa000 && address <= 0xbfff) {
//            cartMbc.mbcWrite(address, value);
//        }
  // 新增支持mbc的bombank切换
  if (pos >= 0) and (pos <= $7FFF) then
  begin
    _pMbc.MbcWrite(pos, val);
  end
  else if (pos >= $A000) and (pos <= $BFFF) then
  begin
    _pMbc.MbcWrite(pos, val);
  end;

  if (pos >= $8000) and (pos <= $9FFF) then
  begin
    _gpu^.vram[pos - $8000] := val;
    if pos <= $97FF then
    begin
      _gpu^.updateTile(pos);
    end;
  end;

  if (pos >= $C000) and (pos <= $DFFF) then
    WRAM[pos - $C000] := val
  else if (pos >= $E000) and (pos <= $FDFF) then
    WRAM[pos - $E000] := val
  else if (pos >= $FE00) and (pos <= $FEFF) then
  begin
    OAM[pos - $FE00] := val;
    _gpu^.buildSprite(pos - $FE00, val);
  end
  else if (pos >= $FF80) and (pos <= $FFFE) then
    HRAM[pos - $FF80] := val
  else if (pos >= $FF00) and (pos <= $FF7F) then
  begin
    case pos of
      $FF00:
        begin
        // joypad
          _gbjoypad.Instance.setGBJoypadMode(val);
        end;
      $FF40:
        begin
          _gpu^.LcdControl_setLcdControl(val);
        end;
      $FF41:
        begin
          _gpu^.LcdStatus_setLcdStatus(val);
        end;
      $FF42:
        begin
          _gpu^.scrollY := val;
        end;
      $FF43:
        begin
          _gpu^.scrollX := val;
        end;
      $FF45:
        begin
          _gpu^.lyc := val;
        end;
      $FF46:
        begin// OAM DMA??
          for I := 0 to 159 do
          begin
            WriteGBMemory($FE00 + I, ReadGBMemory((val shl 8) + I));
          end;
        end;
      $FF47:
        begin
          for I := 0 to 3 do
          begin
            _gpu^.backgroundPalette[I] := _gpu^.palette[(val shr (I * 2)) and 3];
          end;
        end;
      $FF48:
        begin
          for I := 0 to 3 do
          begin
            _gpu^.spritePalette[0][I] := _gpu^.palette[(val shr (I * 2)) and 3];
          end;
        end;
      $FF49:
        begin
          for I := 0 to 3 do
          begin
            _gpu^.spritePalette[1][I] := _gpu^.palette[(val shr (I * 2)) and 3];
          end;
        end;
      $FF0F:
        begin
          _GBInterrupt.Instance.raiseInterruptByReg(val);
        end;
      $FF04:
        begin
          _gbtimer.Instance.clearDivider;
        end;
      $FF05:
        begin
          _gbtimer.Instance.setCounter(val);
        end;
      $FF06:
        begin
          _gbtimer.Instance.setModulo(val);
        end;
      $FF07:
        begin
          _gbtimer.Instance.setContorl(val);
        end;
    else
      IOPort[pos - $FF00] := val;
    end;
  end
  else if pos = $FFFF then
  begin
    _GBInterrupt.Instance.enableInterruptByReg(val);
  end;

//  val := val and $ff;
//  if (pos < $4000) then // 16K ROM
//  begin
//    raise Exception.Create('You can''t write to the ROM');
//  end
//  else if (pos < $8000) then // 16K ROM
//  begin
//    raise Exception.Create('You can''t write to the ROM');
//  end
//  else if (pos < $A000) then // 8K VRAM
//  begin
//    VRAM[pos-$8000] := val;
//  end
//  else if (pos < $C000) then // 8K EXTERNAL RAM
//  begin
//    ExtRAM[pos-$A000] := val;
//  end
//  else if (pos < $D000) then  // 4K WORK RAM
//  begin
//    WRAM0[pos-$C000] := val;
//  end
//  else if (pos < $E000) then  // 4K WORK RAM 2
//  begin
//    WRAM1[pos-$D000] := val;
//  end
//  else if (pos < $FE00) then  // 4K WORK RAM 1 AGAIN
//  begin
////			System.out.println("Don't touch this for gods sake");
//  end
//  else if (pos < $FEA0) then
//  begin
//    OAM[pos-$FE00] := val;
//  end
//  else if (pos < $FF00) then
//  begin
//    // 未使用区域
//  end
//  else if (pos < $FF80) then  // I/O Ports - This means AddressBus BOY
//  begin
////			// Please remove this fugly hack at some point >.<
////			if(pos == 0xFF50) {
////
////				if(val != 0x00) {
////					rom.removeBIOS();
////				} else {
////					rom.overwriteBIOS();
////				}
////			}
////
////			addressBus.writeBus(pos, val);
////			addressBus.updateHardware();
//  end
//  else if (pos < $FFFF) then // High RAM - Don't know what this is for, so throw an exception
//  begin
//    HRAM[pos-$FF80] := val;
//  end
//  else if (pos = $FFFF) then
//  begin
//    // last??
//  end
//  else begin
//    raise Exception.Create('正常不可能到这里的');
//  end;
end;

procedure TGBMemory.WriteGBMemoryWord(pos: Integer; val: Word);
var
  lowVal, upperVal: Integer;
begin
  lowVal := val and $0F;
  upperVal := (val and $F0) shr 8;
  WriteGBMemory(pos, lowVal);
  WriteGBMemory(pos + 1, upperVal);
end;

end.

