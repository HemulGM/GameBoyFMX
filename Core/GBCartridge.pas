unit GBCartridge;

interface

type
  Locale = (Japanese, World, Unknown);

const
  ADDRESS_RAM_SIZE: Integer = $0149;
  ADDRESS_TITLE_START: Integer = $134;
  ADDRESS_TITLE_END: Integer = $143;
  ADDRESS_LOCALE: Integer = $14A;
  ADDRESS_ROM_SIZE: Integer = $148;
  ADDRESS_CART_TYPE: Integer = $0147;
  ADDRESS_LOGO_START: Integer = $104;
  ADDRESS_LOGO_END: Integer = $0133;
  ADDRESS_HEADER_CHECKSUM_EXPECTED: Integer = $014D;
  ADDRESS_HEADER_CHECKSUM_CALCULATED_START: Integer = $0134;
  ADDRESS_HEADER_CHECKSUM_CALCULATED_END: Integer = $014C;

type
  TGBCartirdge = class
  private
  public
    locate: Locale;

  end;

implementation

end.

