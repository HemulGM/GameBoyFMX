unit GBInterruptManager;

interface

uses
  System.SysUtils;

type
  TGBInterrupt = record
    name: string;
    isRaised: Boolean;
    isEnabled: Boolean;
    bit: Integer;
    handler: Integer;
  end;

type
  TArrayGBInterrupt = array of TGBInterrupt;

type
  TGBInterruptManager = class
  private
    _masterEnable: Boolean;
    _upper3bitsForEnableReg: Integer;
    _upper3bitsForFlagReg: Integer;
    _GBInterrupt: array[0..4] of TGBInterrupt;
    class var
      FInstance: TGBInterruptManager;
    class function GetInstance: TGBInterruptManager; static;
  protected
    constructor Create(); overload;
  public
    procedure masterEnable();
    procedure masterDisable();
    function isMasterEnabled(): Boolean;

    procedure raiseInterruptByReg(regValue: Integer);
    procedure enableInterruptByReg(regValue: Integer);
    procedure raiseInterruptByIdx(interruptIdx: Integer);
    procedure enableInterruptByIdx(interruptIdx: Integer);
    procedure disableInterruptByIdx(interruptIdx: Integer);
    procedure clearInterruptByIdx(interruptIdx: Integer);

    function getInterruptsEnabled(): Integer;
    function getInterruptsRaised(): Integer;

    function getAllInterrupt: TArrayGBInterrupt;

    class procedure ReleaseInstance;
    class property Instance: TGBInterruptManager read GetInstance;
  end;

implementation

{ TGBInterruptManager }

procedure TGBInterruptManager.clearInterruptByIdx(interruptIdx: Integer);
begin
  _GBInterrupt[interruptIdx].isRaised := False;
end;

constructor TGBInterruptManager.Create;
begin
  _GBInterrupt[0].name := 'JOYPAD_INPUT';
  _GBInterrupt[0].bit := 16;
  _GBInterrupt[0].handler := $60;
  _GBInterrupt[0].isRaised := False;
  _GBInterrupt[0].isEnabled := False;

  _GBInterrupt[1].name := 'SERIAL_TRANSFER_COMPLETE';
  _GBInterrupt[1].bit := 8;
  _GBInterrupt[1].handler := $58;
  _GBInterrupt[1].isRaised := False;
  _GBInterrupt[1].isEnabled := False;

  _GBInterrupt[2].name := 'TIMER_OVERFLOW';
  _GBInterrupt[2].bit := 4;
  _GBInterrupt[2].handler := $50;
  _GBInterrupt[2].isRaised := False;
  _GBInterrupt[2].isEnabled := False;

  _GBInterrupt[3].name := 'LCDC_STATUS';
  _GBInterrupt[3].bit := 2;
  _GBInterrupt[3].handler := $48;
  _GBInterrupt[3].isRaised := False;
  _GBInterrupt[3].isEnabled := False;

  _GBInterrupt[4].name := 'VBLANK';
  _GBInterrupt[4].bit := 1;
  _GBInterrupt[4].handler := $40;
  _GBInterrupt[4].isRaised := False;
  _GBInterrupt[4].isEnabled := False;

  _masterEnable := False;
  _upper3bitsForEnableReg := 0;
  _upper3bitsForFlagReg := 0;
end;

procedure TGBInterruptManager.disableInterruptByIdx(interruptIdx: Integer);
begin
  _GBInterrupt[interruptIdx].isEnabled := False;
end;

procedure TGBInterruptManager.enableInterruptByIdx(interruptIdx: Integer);
begin
  _GBInterrupt[interruptIdx].isEnabled := True;
end;

procedure TGBInterruptManager.enableInterruptByReg(regValue: Integer);
var
  I: Integer;
begin
  if (regValue > $1f) then
  begin
    _upper3bitsForEnableReg := regValue and $e0;
    regValue := regValue and $1f;
  end;
  for I := 0 to 4 do
  begin
//    if (regValue = _GBInterrupt[i].regVal) then
    if (regValue div _GBInterrupt[I].bit) = 1 then
    begin
      _GBInterrupt[I].isEnabled := True;
    end
    else
    begin
      _GBInterrupt[I].isEnabled := False;
    end;
    regValue := regValue mod _GBInterrupt[I].bit;
  end;
end;

function TGBInterruptManager.getAllInterrupt: TArrayGBInterrupt;
var
  I: Integer;
  ret: TArrayGBInterrupt;
begin
  SetLength(ret, 5);
  for I := 0 to 4 do
    ret[I] := _GBInterrupt[I];
  Result := ret;
end;

class function TGBInterruptManager.GetInstance: TGBInterruptManager;
begin
  if FInstance = nil then
    FInstance := TGBInterruptManager.Create;
  Result := FInstance;
end;

function TGBInterruptManager.getInterruptsEnabled: Integer;
var
  I, ret: Integer;
begin
  ret := 0;
  for I := 0 to 4 do
  begin
    if (_GBInterrupt[I].isEnabled) then
    begin
      ret := ret or _GBInterrupt[I].bit;
    end;
  end;
  ret := ret or _upper3bitsForEnableReg;
  Result := ret;
end;

function TGBInterruptManager.getInterruptsRaised: Integer;
var
  I, ret: Integer;
begin
  ret := 0;
  for I := 0 to 4 do
  begin
    if (_GBInterrupt[I].isRaised) then
    begin
      ret := ret or _GBInterrupt[I].bit;
    end;
  end;
  ret := ret or _upper3bitsForFlagReg;
  Result := ret;
end;

function TGBInterruptManager.isMasterEnabled: Boolean;
begin
  Result := _masterEnable;
end;

procedure TGBInterruptManager.masterDisable;
begin
  _masterEnable := False;
end;

procedure TGBInterruptManager.masterEnable;
begin
  _masterEnable := True;
end;

procedure TGBInterruptManager.raiseInterruptByIdx(interruptIdx: Integer);
begin
  _GBInterrupt[interruptIdx].isRaised := True;
end;

procedure TGBInterruptManager.raiseInterruptByReg(regValue: Integer);
var
  I: Integer;
begin
  if (regValue > $1F) then
  begin
//            log.error(registerValue + " is out of range of possible interrupt flag register values");
    _upper3bitsForFlagReg := regValue and $E0;
    regValue := regValue and $1F;
  end;
  for I := 0 to 4 do
  begin
//    if (regValue = _GBInterrupt[i].regVal) then
    if (regValue div _GBInterrupt[I].bit) = 1 then
    begin
      _GBInterrupt[I].isRaised := True;
    end
    else
    begin
      _GBInterrupt[I].isRaised := False;
    end;
    regValue := regValue mod _GBInterrupt[I].bit;
  end;
end;

class procedure TGBInterruptManager.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

end.

