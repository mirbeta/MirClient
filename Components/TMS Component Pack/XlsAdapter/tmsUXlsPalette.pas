unit tmsUXlsPalette;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsXlsMessages, tmsUFlxMessages;
type
  TPaletteRecord= class(TBaseRecord)
  private
    function GetColor(index: integer): LongWord;
    procedure SetColor(index: integer; const Value: LongWord);
    function GetCount: word;
  public
    property Count: word read GetCount;
    property Color[index: integer]: LongWord read GetColor write SetColor;
    constructor CreateStandard;
  end;

  function StandardPalette(const Index: integer): LongWord;
implementation

{ TPaletteRecord }

constructor TPaletteRecord.CreateStandard;
var
  aData: PArrayOfByte;
  aDataSize:integer;
  i: integer;
begin
  aDataSize:=SizeOf(Word)+High(TColorPaletteRange)*SizeOf(LongWord);
  GetMem(aData, aDataSize);
  try
    SetWord(aData,0,High(TColorPaletteRange));
    for i:=Low(TColorPaletteRange)-1 to High(TColorPaletteRange)-1 do SetLongWord(aData,2+i*SizeOf(LongWord), StandardPalette(i));
  except
    FreeAndNil(aData);
    raise;
  end; //Except
  inherited Create(xlr_PALETTE, aData, aDataSize);
end;

function TPaletteRecord.GetColor(Index: integer): LongWord;
begin
  if (Index>=Count) or (Index<0) then raise Exception.CreateFmt(ErrXlsIndexOutBounds,[Index,'Palette Color Index',0, Count-1]);
  Result:=GetLongWord(Data, 2+Index*4);
end;

function TPaletteRecord.GetCount: word;
begin
  Result:=GetWord(Data, 0);
end;

procedure TPaletteRecord.SetColor(index: integer; const Value: LongWord);
begin
  if (Index>=Count) or (Index<0) then raise Exception.CreateFmt(ErrTooManyEntries,[Index, Count-1]);
  SetLongWord(Data, 2+Index*4, Value);
end;

function StandardPalette(const Index: integer): LongWord;
const
  StdArray: array[Low(TColorPaletteRange)-1..High(TColorPaletteRange)-1] of LongWord=
                                     (  0,
                                        16777215,
                                        255,
                                        65280,
                                        16711680,
                                        65535,
                                        16711935,
                                        16776960,
                                        128,
                                        32768,
                                        8388608,
                                        32896,
                                        8388736,
                                        8421376,
                                        12632256,
                                        8421504,
                                        16751001,
                                        6697881,
                                        13434879,
                                        16777164,
                                        6684774,
                                        8421631,
                                        13395456,
                                        16764108,
                                        8388608,
                                        16711935,
                                        65535,
                                        16776960,
                                        8388736,
                                        128,
                                        8421376,
                                        16711680,
                                        16763904,
                                        16777164,
                                        13434828,
                                        10092543,
                                        16764057,
                                        13408767,
                                        16751052,
                                        10079487,
                                        16737843,
                                        13421619,
                                        52377,
                                        52479,
                                        39423,
                                        26367,
                                        10053222,
                                        9868950,
                                        6697728,
                                        6723891,
                                        13056,
                                        13107,
                                        13209,
                                        6697881,
                                        10040115,
                                        3355443);

begin
  if (Index<0) or (Index>High(TColorPaletteRange)-1) then Raise Exception.CreateFmt(ErrXlsIndexOutBounds,[Index,'Palette index' ,0,High(TColorPaletteRange)-1]);
  Result:= StdArray[Index];
end;
end.
