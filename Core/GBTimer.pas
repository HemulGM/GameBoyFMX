unit GBTimer;

interface

uses
  System.SysUtils, GBInterruptManager;

type
  TGBTimer = class
  private
    _div, _tac, _tma, _tima, _ticksSinceOverflow: Integer;
    _previousBit, _overflow: Boolean;
    FREQ_TO_BIT: array[0..3] of Integer;
    procedure incTima();
    procedure updateDiv(newDiv: Integer);

    class var
      FInstance: TGBTimer;
    class function GetInstance: TGBTimer; static;
  protected
    constructor Create(); overload;
  public
    procedure step(numSteps: Integer);
    procedure tick();
    procedure clearDivider();
    function getDivider(): Integer;
    function getCounter(): Integer;
    procedure setCounter(value: Integer);
    function getModulo(): Integer;
    procedure setModulo(value: Integer);
    function getControl(): Integer;
    procedure setContorl(value: Integer);
    procedure setDivBypass(value: Integer);
    class procedure ReleaseInstance;
    class property Instance: TGBTimer read GetInstance;
  end;

implementation

{ TGBTimer }

procedure TGBTimer.clearDivider;
begin
  updateDiv(0);
end;

constructor TGBTimer.Create;
begin
  FREQ_TO_BIT[0] := 9; // (9, 3, 5, 7);
  FREQ_TO_BIT[1] := 3;
  FREQ_TO_BIT[2] := 5;
  FREQ_TO_BIT[3] := 6;
  _div := 0;
  _tac := 0;
  _tma := 0;
  _tima := 0;
  _ticksSinceOverflow := 0;
  _previousBit := False;
  _overflow := False;
end;

function TGBTimer.getControl: Integer;
begin
  Result := _tac;
end;

function TGBTimer.getCounter: Integer;
begin
  Result := _tima;
end;

function TGBTimer.getDivider: Integer;
begin
  Result := _div shr 8;
end;

class function TGBTimer.GetInstance: TGBTimer;
begin
  if FInstance = nil then
    FInstance := TGBTimer.Create;
  Result := FInstance;
end;

function TGBTimer.getModulo: Integer;
begin
  Result := _tma;
end;

procedure TGBTimer.incTima;
begin
  Inc(_tima);
  _tima := _tima mod $100;
  if (_tima = 0) then
  begin
    _overflow := True;
    _ticksSinceOverflow := 0;
  end;
end;

class procedure TGBTimer.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TGBTimer.setContorl(value: Integer);
begin
  _tac := value;
end;

procedure TGBTimer.setCounter(value: Integer);
begin
  if (_ticksSinceOverflow < 5) then
  begin
    _tima := value;
    _overflow := False;
    _ticksSinceOverflow := 0;
  end;
end;

procedure TGBTimer.setDivBypass(value: Integer);
begin
  _div := value;
end;

procedure TGBTimer.setModulo(value: Integer);
begin
  _tma := value;
end;

procedure TGBTimer.step(numSteps: Integer);
var
  i: Integer;
begin
  for i := 0 to numSteps - 1 do
    tick();
end;

procedure TGBTimer.tick;
begin
  updateDiv((_div + 1) and $ffff);
  if (_overflow) then
  begin
    Inc(_ticksSinceOverflow);
    if (_ticksSinceOverflow = 4) then
    begin
      // 还是要实现中断
      TGBInterruptManager.Instance.raiseInterruptByIdx(2); // 'TIMER_OVERFLOW';
    end;
    if (_ticksSinceOverflow = 5) then
      _tima := _tma;
    if (_ticksSinceOverflow = 6) then
    begin
      _tima := _tma;
      _overflow := false;
      _ticksSinceOverflow := 0;
    end;
  end;
end;

procedure TGBTimer.updateDiv(newDiv: Integer);
var
  bitPos: Integer;
  bit: Boolean;
begin
  _div := newDiv;
  bitPos := FREQ_TO_BIT[_tac and 3];
  if (_div and (1 shl bitPos) <> 0) then
    bit := True
  else
    bit := False;
  // bug fix ,少处理一块逻辑
  if (_tac and (1 shl 2)) <> 0 then
    bit := bit and True
  else
    bit := bit and False;
//  bit &= (tac & (1 << 2)) != 0;
  if (not bit) and _previousBit then
  begin
    incTima();
  end;
  _previousBit := bit;
end;

end.

