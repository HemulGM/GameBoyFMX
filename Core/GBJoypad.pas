unit GBJoypad;

interface

uses
  System.SysUtils, GBInterruptManager;
//type GBKeys = (A=90,B=88,START=10,SELECT=16,UP=38,DOWN=40,LEFT=37,RIGHT=39);

{$SCOPEDENUMS ON}

type
  TGBKey = (UP = 0, DOWN = 1, LEFT = 2, RIGHT = 3, A = 4, B = 5, SELECT = 6, START = 7);

type
  TGBJoypad = class
  private
    aPressed: Boolean;
    bPressed: Boolean;
    startPressed: Boolean;
    selectPressed: Boolean;
    upPressed: Boolean;
    downPressed: Boolean;
    leftPressed: Boolean;
    rightPressed: Boolean;
    isDpadMode: Boolean;
    class var
      FInstance: TGBJoypad;
    class function GetInstance: TGBJoypad; static;
  protected
    constructor Create(); overload;
  public
    _GBInterrupt: TGBInterruptManager;
    GBKeys: array[TGBKey] of Integer;
    function getGBKeyPressed(): Integer;
    procedure setGBJoypadMode(value: Integer);
    procedure GBKeyDown(Key: Integer);//按下
    procedure GBKeyUp(Key: Integer);//放开
    class procedure ReleaseInstance;
    class property Instance: TGBJoypad read GetInstance;
  end;

implementation

{ TGBJoypad }

constructor TGBJoypad.Create;
begin
  aPressed := False;
  bPressed := False;
  startPressed := False;
  selectPressed := False;
  upPressed := False;
  downPressed := False;
  leftPressed := False;
  rightPressed := False;

    // UP=87,DOWN=83,LEFT=65,RIGHT=68
  GBKeys[TGBKey.UP] := 87;
  GBKeys[TGBKey.DOWN] := 83;
  GBKeys[TGBKey.LEFT] := 65;
  GBKeys[TGBKey.RIGHT] := 68;
    // A=74,B=75,SELECT=90,START=88
  GBKeys[TGBKey.A] := 74;
  GBKeys[TGBKey.B] := 75;
  GBKeys[TGBKey.SELECT] := 90;
  GBKeys[TGBKey.START] := 88;
end;

procedure TGBJoypad.GBKeyDown(Key: Integer);
begin
    // UP=87,DOWN=83,LEFT=65,RIGHT=68
  if Key = GBKeys[TGBKey.A] then
  begin
    if not isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    aPressed := True;
  end
  else if Key = GBKeys[TGBKey.B] then
  begin
    if not isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    bPressed := True;
  end
  else if Key = GBKeys[TGBKey.START] then
  begin
    if not isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    startPressed := True;
  end
  else if Key = GBKeys[TGBKey.SELECT] then
  begin
    if not isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    selectPressed := True;
  end
  else if Key = GBKeys[TGBKey.UP] then
  begin
    if isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    upPressed := True;
  end
  else if Key = GBKeys[TGBKey.DOWN] then
  begin
    if isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    downPressed := True;
  end
  else if Key = GBKeys[TGBKey.LEFT] then
  begin
    if isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    leftPressed := True;
  end
  else if Key = GBKeys[TGBKey.RIGHT] then
  begin
    if isDpadMode then
      _GBInterrupt.Instance.raiseInterruptByIdx(0); //JoyPad_Input
    rightPressed := True;
  end;
end;

procedure TGBJoypad.GBKeyUp(Key: Integer);
begin
  if Key = GBKeys[TGBKey.A] then
  begin
    aPressed := False;
  end
  else if Key = GBKeys[TGBKey.B] then
  begin
    bPressed := False;
  end
  else if Key = GBKeys[TGBKey.START] then
  begin
    startPressed := False;
  end
  else if Key = GBKeys[TGBKey.SELECT] then
  begin
    selectPressed := False;
  end
  else if Key = GBKeys[TGBKey.UP] then
  begin
    upPressed := False;
  end
  else if Key = GBKeys[TGBKey.DOWN] then
  begin
    downPressed := False;
  end
  else if Key = GBKeys[TGBKey.LEFT] then
  begin
    leftPressed := False;
  end
  else if Key = GBKeys[TGBKey.RIGHT] then
  begin
    rightPressed := False;
  end;
end;

function TGBJoypad.getGBKeyPressed: Integer;
var
  retval: Integer;
begin
  retval := $CF;
  if (isDpadMode) then//按的是十字键
  begin
    retval := retval or $20;
    if downPressed then// Bit 3 - P13 Input Down  (0=Pressed)
      retval := retval and $F7
    else
      retval := retval and $FF;
    if upPressed then// Bit 2 - P12 Input Up    (0=Pressed)
      retval := retval and $FB
    else
      retval := retval and $FF;

    if leftPressed then// Bit 1 - P11 Input Left  (0=Pressed)
      retval := retval and $FD
    else
      retval := retval and $FF;
    if rightPressed then// Bit 0 - P10 Input Right (0=Pressed)
      retval := retval and $FE
    else
      retval := retval and $FF;
  end
  else
  begin//按的是按钮键
    retval := retval or $20;
    if startPressed then//  Bit 3 - P13 Input Start    (0=Pressed)
      retval := retval and $F7
    else
      retval := retval and $FF;
    if selectPressed then//Bit 2 - P12 Input Select   (0=Pressed)
      retval := retval and $FB
    else
      retval := retval and $FF;

    if bPressed then//Bit 1 - P11 Input Button B (0=Pressed)
      retval := retval and $FD
    else
      retval := retval and $FF;
    if aPressed then// Bit 0 - P10 Input Button A (0=Pressed)
      retval := retval and $FE
    else
      retval := retval and $FF;
  end;
  Result := retval;
end;

class function TGBJoypad.GetInstance: TGBJoypad;
begin
  if FInstance = nil then
    FInstance := TGBJoypad.Create;
  Result := FInstance;
end;

class procedure TGBJoypad.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TGBJoypad.setGBJoypadMode(value: Integer);
begin
  value := value and $30;
  if value = $20 then
    isDpadMode := True
  else if value = $10 then
    isDpadMode := False;
end;

end.

