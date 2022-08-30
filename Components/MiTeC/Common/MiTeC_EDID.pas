{*******************************************************}
{               MiTeC Common Routines                   }
{                   EDID decoding                       }
{                                                       }
{          Copyright (c) 1997-2019 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_EDID;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

const
  EDID_Header = $00FFFFFFFFFFFF00;

type
  TTimingDescriptor = record
    Flag: array[0..4] of Byte;
    Data: array[0..13] of Byte;
  end;

  TEDID = record
// Header
    Header: array[0..7] of Byte;
// Vendor / Product ID / EDID Version
    EISA_ManufacturerCode: Word;
    ProductCode: Word;
    SerialNumber: Cardinal;
    WeekOfManufacture: Byte;
    YearOfManufacture: Byte;
    EDID_Version: Byte;
    EDID_Revision: Byte;
// Display Parameter
    VideoInputDefinition: Byte;
    MaxHImageSize: Byte; //cm
    MaxVImageSize: Byte; //cm
    DisplayGamma: Byte; //=(gamma*100)-100
    Features: Byte;
// Panel Color Coordinates
    PanelColorCoordinates: array[0..9] of Byte;
// Established Timings
    EstablishedTimings: array[0..2] of Byte;
// Standard Timing ID
    StandardTimingID: array[0..15] of Byte;
// Timing Descriptor #1
    TD1: array[0..17] of Byte;
// Timing Descriptor #2 (MANUFACTURER SPECIFIED RANGE TIMING Descriptor)
    TD2: TTimingDescriptor;
// Timing Descriptor #3 (Supplier Name)
    TD3: TTimingDescriptor;
// Timing Descriptor #4 (Supplier P/N)
    TD4: TTimingDescriptor;

    ExtensionFlag: Byte; //(# of optional 128-byte EDID extension blocks to follow, typ=0)
    Checksum: Byte; //(the 1-byte sum of all 128 bytes in this EDID block shall equal zero)
  end;

  TEDIDRecord= record
    Name,
    ProductNumber: string;
    Width,
    Height: Byte;
    SerialNumber: Cardinal;
    Week,
    Year: WORD;
    ManufacturerCode,
    ProductCode: Word;
    Gamma: Double;
    Version: string;
    PNPID: string;
  end;

procedure DecodeEDID(AData: PAnsiChar; var ARecord: TEDIDRecord);

implementation

uses MiTeC_StrUtils;

procedure DecodeEDID(AData: PAnsiChar; var ARecord: TEDIDRecord);
var
  edid: TEDID;
  sa: Cardinal;
  b1,b2: Byte;
  s: string;
begin

  Move(AData^,edid,SizeOf(edid));
  if edid.EstablishedTimings[2]=0 then begin
    sa:=$36;
    Move(AData[sa],edid.TD1,SizeOf(edid.TD1));
    sa:=$48;
    Move(AData[sa],edid.TD2,SizeOf(edid.TD2));
    sa:=$5A;
    Move(AData[sa],edid.TD3,SizeOf(edid.TD3));
    sa:=$6C;
    Move(AData[sa],edid.TD4,SizeOf(edid.TD4));
  end;
  ARecord.Name:='';
  ARecord.ProductNumber:='';
  case edid.TD2.Flag[3] of
    $FC: ARecord.Name:=Trim(GetStrFromBuf(edid.TD2.Data,SizeOf(edid.TD2.Data)));
    $FF: ARecord.ProductNumber:=Trim(GetStrFromBuf(edid.TD2.Data,SizeOf(edid.TD2.Data)));
  end;
  case edid.TD3.Flag[3] of
    $FC: begin
      s:=Trim(GetStrFromBuf(edid.TD3.Data,SizeOf(edid.TD3.Data)));
      if not SameText(ARecord.Name,s) then
        ARecord.Name:=ARecord.Name+s;
    end;
    $FF: begin
      s:=Trim(GetStrFromBuf(edid.TD3.Data,SizeOf(edid.TD3.Data)));
      if not SameText(ARecord.ProductNumber,s) then
        ARecord.ProductNumber:=ARecord.ProductNumber+s;
    end;
  end;
  case edid.TD4.Flag[3] of
    $FC: begin
      s:=Trim(GetStrFromBuf(edid.TD4.Data,SizeOf(edid.TD4.Data)));
      if Pos(Uppercase(s),UpperCase(ARecord.Name))=0 then
        ARecord.Name:=ARecord.Name+s;
    end;
    $FF: begin
      s:=Trim(GetStrFromBuf(edid.TD4.Data,SizeOf(edid.TD4.Data)));
      if Pos(Uppercase(s),UpperCase(ARecord.ProductNumber))=0 then
        ARecord.ProductNumber:=ARecord.ProductNumber+s;
    end;
  end;
  ARecord.Width:=edid.MaxHImageSize;
  ARecord.Height:=edid.MaxVImageSize;
  ARecord.SerialNumber:=edid.SerialNumber;
  ARecord.Week:=edid.WeekOfManufacture;
  ARecord.Year:=edid.YearOfManufacture+1990;
  ARecord.ManufacturerCode:=edid.EISA_ManufacturerCode;
  ARecord.ProductCode:=edid.ProductCode;
  ARecord.Gamma:=(edid.DisplayGamma+100)/100;
  ARecord.Version:=Format('%d.%d',[edid.EDID_Version,edid.EDID_Revision]);
  b1:=Lo(edid.EISA_ManufacturerCode);
  b2:=Hi(edid.EISA_ManufacturerCode);
  ARecord.PNPID:=Chr(((b1 and $7C) shr 2)+$40);
  ARecord.PNPID:=ARecord.PNPID+Chr(((b1 and $03) shl 3)+((b2 and $E0) shr 5)+$40);
  ARecord.PNPID:=ARecord.PNPID+Chr((b2 and $1F)+$40);
end;

end.
