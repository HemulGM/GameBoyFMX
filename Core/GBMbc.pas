unit GBMbc;

interface

uses
  GBRom;

type
  MbcType = (ROM_ONLY, MBC1, MBC2, MBC3, MBC5, MBC07, MMM01, HuC1, HuC3);

type
  TGBMbc = class
  private
    _pgbrom: PGBRom;
    ramEnabled, hasRam: Boolean;
    isRomMode: Boolean;
    ram: array of Integer;

  public
    romBankSelected, ramBankSelected: Integer;
    function MbcRead(address: Integer): Integer;
    procedure MbcWrite(address, value: Integer);
    constructor Create(pgbrom: PGBRom); overload;
  end;

type
  PGBMbc = ^TGBMbc;

implementation

{ TGBMbc }

constructor TGBMbc.Create(pgbrom: pgbrom);
begin
  _pgbrom := pgbrom;
  romBankSelected := 1;
  ramBankSelected := 0;
  ramEnabled := False;
  hasRam := False;
  isRomMode := True;
end;

function TGBMbc.MbcRead(address: Integer): Integer;
var
  effectiveAddress: Integer;
begin
  case _pgbrom^.GetCartridgeType.id of
    $00:
      begin// ROM_ONLY
        Result := _pgbrom^.romData[address];
      end;
    $01:
      begin// MBC1
        Result := 0;
        if address < $4000 then
        begin
          Result := _pgbrom^.romData[address];
        end
        else if address < $8000 then
        begin
          effectiveAddress := (romBankSelected * $4000) + (address - $4000);
          Result := _pgbrom^.romData[effectiveAddress];
        end
        else if (address >= $A000) and (address <= $BFFF) then
        begin
          if (not ramEnabled) or (not hasRam) then
          begin
            Result := $FF;
          end
          else if (ramBankSelected = 0) then
          begin
            Result := ram[address - $A000];
          end
          else
          begin
            effectiveAddress := (ramBankSelected * $2000) + (address - $A000);
            Result := ram[effectiveAddress];
          end;
        end;
      end;
  else
    // this MBC is not support
    Result := 0;
  end;
end;

procedure TGBMbc.MbcWrite(address, value: Integer);
var
  effectiveAddress: Integer;
begin
  case _pgbrom^.GetCartridgeType.id of
    $00:
      begin// ROM_ONLY
      // not support write to rom.
      end;
    $01:
      begin// MBC1
        if address <= $1FFF then
        begin
          if (value and $0F) = $A then
            ramEnabled := True
          else
            ramEnabled := False;
        end
        else if address <= $3FFF then
        begin
          romBankSelected := value and $1F;
          if romBankSelected = 0 then
            Inc(romBankSelected);
        end
        else if address <= $5FFF then
        begin
          if isRomMode then
          begin
            romBankSelected := romBankSelected and $1F;
            romBankSelected := romBankSelected or ((value and $3) shl 5);
            if (romBankSelected = 0) or (romBankSelected = $20)
              or (romBankSelected = $40) or (romBankSelected = $60) then
            begin
              Inc(romBankSelected);
            end;
          end
          else
          begin
            ramBankSelected := value and $3;
          end;
        end
        else if address <= $7FFF then
        begin
          if value = 0 then// 0=rom,1=ram
            isRomMode := True
          else
            isRomMode := False;
          if isRomMode then
            ramBankSelected := 0
          else
            romBankSelected := romBankSelected and $1F;
        end
        else if (address <= $BFFF) and (address >= $A000) then
        begin
          if (not ramEnabled) or (not hasRam) then
            Exit
          else if ramBankSelected = 0 then
          begin
            ram[address - $A000] := value; // 后续支持记忆卡
          end
          else
          begin
            effectiveAddress := (ramBankSelected * $2000) + (address - $A000);
            ram[effectiveAddress] := value;
          end;
        end;
      end;
  end;
end;

end.

