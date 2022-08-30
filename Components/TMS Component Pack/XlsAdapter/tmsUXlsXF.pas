unit tmsUXlsXF;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses Classes, SysUtils, tmsUXlsBaseRecords, tmsUXlsBaseRecordLists, tmsXlsMessages,
     {$IFDEF DELPHIXE3UP} System.Contnrs, {$ENDIF}
     tmsUFlxFormats, tmsUXlsStrings, tmsUFlxMessages;

type
  TXFDat=packed record
    Font: word;       //0
    Format: word;     //2
    Options4: word;    //4
    Options6: word;  //6
    Options8: word;  //8
    Options10: word;  //10
    Options12: word;     //12
    Options14: LongWord; //14
    Options18: Word; //18
  end;
  PXFDat=^TXFDat;

  TFontRecordList=class;
  TFormatRecordList=class;

  TXFRecord=class(TBaseRecord)
  public
    function CellPattern: integer;
    function CellFgColorIndex: integer;
    function CellBgColorIndex: integer;

    function FontIndex: integer;
    function FormatIndex: integer;
    function GetActualFontIndex(const FontList: TFontRecordList): integer;

    function GetBorderStyle(const aPos: integer; const FirstBit: byte):TFlxBorderStyle;
    function GetBorderColorIndex(const aPos: integer; const FirstBit: byte):integer;
    function GetBorderStyleExt(const aPos: integer; const FirstBit: byte):TFlxBorderStyle;
    function GetBorderColorIndexExt(const aPos: integer; const FirstBit: byte):integer;

    function DiagonalStyle: TFlxDiagonalBorder;

    function VAlign: TVFlxAlignment;
    function HAlign: THFlxAlignment;

    procedure FillMisc(out Locked, Hidden: boolean;out Parent: integer;
                       out WrapText, ShrinkToFit: boolean; out Rotation: byte;
                       out Indent: byte);

    constructor CreateFromFormat(const Fmt: TFlxFormat; const FontList: TFontRecordList; const FormatList: TFormatRecordList);
    function FlxFormat(const FontList: TFontRecordList; const FormatList: TFormatRecordList): TFlxFormat;
    procedure FillUsedColors(const UsedColors: BooleanArray; const FontList: TFontRecordList);

    function Rotation: integer;
    function WrapText: boolean;
  end;

  TXFRecordList= class(TBaseRecordList)  //Items are TXFRecord
  {$INCLUDE TXFRecordListHdr.inc}
  public
    function FindFormat(const XF: TXFRecord; out Index: integer): boolean;
    function GetUsedColors(const ColorCount: integer; const FontList: TFontRecordList): BooleanArray;
  end;

//------------------------------------------------- FONT
  TFontDat=packed record
    Height: word;
    GrBit: word;
    ColorIndex: word;
    BoldStyle: word;
    SuperSub: word;
    Underline: byte;
    Family: byte;
    CharSet: byte;
    Reserved: byte;
    //Font name is not included
  end;
  PFontDat=^TFontDat;

  TFontRecord= class(TBaseRecord)
    function Name: UTF16String;
    function Height: integer;
    function ColorIndex: integer;
    function Family: byte;
    function Charset: byte;

    function Style: SetOfTFlxFontStyle;
    function Underline: TFlxUnderline;

    constructor CreateFromFlxFont(const aFont: TFlxFont);
  public
    function FlxFont: TFlxFont;
  end;

  TFontRecordList=class(TBaseRecordList)  //Items are TFontRecord
  {$INCLUDE TFontRecordListHdr.inc}
  public
    function AddFont(const aFont: TFlxFont): integer;
  end;

///------------------------------------------------ Style
  TStyleRecord= class(TBaseRecord)
  end;

//------------------------------------------------- FORMAT
  TFormatRecord= class(TBaseRecord)
  public
    function FormatId: integer;
    function Value: UTF16String;
    constructor CreateFromData(const Fmt: UTF16String; const NewID: integer);
  end;

  TFormatRecordList=class(TBaseRecordList)
  private
    function GetFormat(FormatId: integer): UTF16String;  //Items are TFormatRecord
  {$INCLUDE TFormatRecordListHdr.inc}
  public
    property Format[index: integer]: UTF16String read GetFormat; default;
    function AddFormat(const Fmt: UTF16String): integer;
  end;

  function XlsBuiltInFormats(const z: integer): UTF16String;
implementation
  {$INCLUDE TXFRecordListImp.inc}
  {$INCLUDE TFontRecordListImp.inc}
  {$INCLUDE TFormatRecordListImp.inc}

const
  XlsBuiltInFormatsUs: array[0..49] of UTF16String=
  ('', '0', '0.00','#,##0','#,##0.00',                              //0..4
   '', '', '', '',                                                   //5..8  Contained in file
   '0%','0.00%','0.00E+00','?/?','# ??/??',                         //9..13
   'mm/dd/YYYY','DD-MMM-YY','DD-MMM','MMM-YY',                      //14..17
   'h:mm AM/PM','h:mm:ss AM/PM','hh:mm','hh:mm:ss',                 //18..21
   'mm/dd/YYYY hh:mm',                                              //22
   '','','','','','','','','','','','','','',                       //23..36 Reserved
   '#,##0 _$;-#,##0 _$','#,##0 _$;[Red]-#,##0 _$',              //37..38
   '#,##0.00 _$;-#,##0.00 _$','#,##0.00 _$;[Red]-#,##0.00 _$',  //39..40
   '','','','',                                                     //41..44 contained in file
   'mm:ss','[h]:mm:ss','mm:ss,0','##0.0E+0','@'                 //45..49
  );

function XlsBuiltInFormats(const z: integer): UTF16String;
var
  i: integer;
  s, Sep, Hour: UTF16String;
  c: UTF16Char;
  FormatSettings: PFormatSettings;
begin
  FormatSettings := GetDefaultLocaleFormatSettings;
  if (z = 14) then //Regional date.
  begin
      s := FormatSettings.ShortDateFormat;
      Sep := FormatSettings.DateSeparator;
      //It looks Excel will only show 3 formats:
      // m/d/y
      // d/m/y
      // y/m/d
      //depending on which thing you begin. yy and yyyy are both allowed.

      for i:=1 to Length(s) do
      begin
          c := s[i];
          if (c='y') or (c = 'Y') then begin; Result:= 'YYYY' + Sep + 'mm' + Sep + 'dd'; exit; end;
          if (c='m') or (c = 'M') then begin; Result:= 'mm' + Sep + 'dd' + Sep + 'YYYY'; exit; end;
          if (c='d') or (c = 'D') then begin; Result:= 'dd' + Sep + 'mm' + Sep + 'YYYY'; exit; end;
      end;
  end;
  if (z = 22) then //Regional date time
  begin
      s := FormatSettings.ShortDateFormat;
      Sep := FormatSettings.DateSeparator;
      //It looks Excel will only show 3 formats:
      // m/d/y
      // d/m/y
      // y/m/d
      //depending on which thing you begin. yy and yyyy are both allowed.

      Hour := ' hh' + FormatSettings.TimeSeparator + 'mm';

      for i:=1 to Length(s) do
      begin
          c := s[i];
          if (c='y') or (c = 'Y') then begin; Result:= 'YYYY' + Sep + 'mm' + Sep + 'dd' + Hour; exit; end;
          if (c='m') or (c = 'M') then begin; Result:= 'mm' + Sep + 'dd' + Sep + 'YYYY' + Hour; exit; end;
          if (c='d') or (c = 'D') then begin; Result:= 'dd' + Sep + 'mm' + Sep + 'YYYY' + Hour; exit; end;
      end;
  end;

  Result:= XlsBuiltInFormatsUs[z];
end;

{ TXFRecord }

function TXFRecord.CellBgColorIndex: integer;
begin
  Result:= (PXFDat(Data).Options18 and $3F80) shr 7 -7;
end;

function TXFRecord.CellFgColorIndex: integer;
begin
  Result:= PXFDat(Data).Options18 and $7F -7;
end;

function TXFRecord.CellPattern: integer;
begin
  Result:= PXFDat(Data).Options14 and $FC000000 shr 26;
end;

function BoolToBit(const Value: boolean; const ofs: integer): integer;
begin
  if Value then Result:=1 shl ofs else Result:=0;
end;

constructor TXFRecord.CreateFromFormat(const Fmt: TFlxFormat; const FontList: TFontRecordList; const FormatList: TFormatRecordList);
var
  TempData: PXFDat;
  bl, br, bb, bt: integer;
begin
  GetMem (TempData, SizeOf(TXFDat));
  try
    TempData.Font:= FontList.AddFont(Fmt.Font);
    TempData.Format:=FormatList.AddFormat(Fmt.Format);
    TempData.Options4:= BoolToBit(Fmt.Locked,0)+
                                  BoolToBit(Fmt.Hidden,1)+
                                  0 shl 2+ //Cell style
                                  0 shl 3+ //123 lotus
    //                              (Fmt.Parent shl 4) and $FFF0;
                                  0;  //No parents... they are too confusing

    TempData.Options4:=TempData.Options4 and not $0004; //not style
    TempData.Options6:=integer(Fmt.HAlignment)+
                       BoolToBit(Fmt.WrapText, 3)+
                       integer(Fmt.VAlignment)shl 4+
                       (Fmt.Rotation shl 8)and $FF00;

    TempData.Options8:=Fmt.Indent and $F+
                       BoolToBit(Fmt.ShrinkToFit, 4)+
                       0+ //mergecell
                       0+ //readingOrder
                       $0400+ //fmt not equal to parent
                       $0800+ //Font not equal to parent
                       $1000+$2000+$4000+$8000; //Many things not equal to parent...
                       //PENDING: agregar

    TempData.Options10:=integer(Fmt.Borders.Left.Style)+
                        integer(Fmt.Borders.Right.Style) shl 4+
                        integer(Fmt.Borders.Top.Style) shl 8+
                        integer(Fmt.Borders.Bottom.Style) shl 12;

    bl:=(Fmt.Borders.Left.ColorIndex+7) and $7F;
    if bl<8 then bl:=8; if bl>56+8 then bl:=56+8;
    br:=(Fmt.Borders.Right.ColorIndex+7) and $7F;
    if br<8 then br:=8; if br>56+8 then br:=56+8;
    TempData.Options12:=bl +
                        (br) shl 7+
                        integer(Fmt.Borders.DiagonalStyle) shl 14;

    bt:=(Fmt.Borders.Top.ColorIndex+7) and $7F;
    if bt<8 then bt:=8; if bt>56+8 then bt:=56+8;
    bb:=(Fmt.Borders.Bottom.ColorIndex+7) and $7F;
    if bb<8 then bb:=8; if bb>56+8 then bb:=56+8;
    TempData.Options14:= bt+
                         bb shl 7+
                        ((Fmt.Borders.Diagonal.ColorIndex+7) and $7F) shl 14+
                        integer(Fmt.Borders.Diagonal.Style) shl 21+
                        (Fmt.FillPattern.Pattern-1) shl 26;

    TempData.Options18:= (Fmt.FillPattern.FgColorIndex+7) and $7F +
                         ((Fmt.FillPattern.BgColorIndex+7) and $7F) shl 7+
                         0; //Attached to pivot table


    inherited Create(xlr_XF, PArrayOfByte(TempData), SizeOf(TXFDat));
  except
    FreeMem(TempData);
    raise;
  end;
end;

function TXFRecord.DiagonalStyle: TFlxDiagonalBorder;
begin
  Result:=TFlxDiagonalBorder((GetLongWord(Data,12) shr 14) and  3);
end;

procedure TXFRecord.FillMisc(out Locked, Hidden: boolean;
  out Parent: integer; out WrapText, ShrinkToFit: boolean; out Rotation, Indent: byte);
begin
  Locked:=PXFDat(Data).Options4 and $1 = $1;
  Hidden:=PXFDat(Data).Options4 and $2 = $2;
  Parent:=PXFDat(Data).Options4 and $FFF0;

  WrapText:= PXFDat(Data).Options6 and $8 = $8;

  ShrinkToFit:= PXFDat(Data).Options8 and $10 = $10;

  Rotation:= Hi(PXFDat(Data).Options6 and $FF00);
  Indent:= PXFDat(Data).Options8 and $F;

end;

function TXFRecord.GetActualFontIndex(const FontList: TFontRecordList): integer;
begin
  Result := FontIndex;
  if Result >= 4 then
    Dec(Result);

  if (Result < 0) or (Result >= FontList.Count) then
    Result := 0;
end;

procedure TXFRecord.FillUsedColors(const UsedColors: BooleanArray; const FontList: TFontRecordList);
var
  i: integer;
  bs: TFlxBorderStyle;
  Pattern: TFlxPatternStyle;
begin
  bs := GetBorderStyle(10, 0);
  if bs <> fbs_None then
  begin
    i := GetBorderColorIndex(12, 0);
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

  end;

  bs := GetBorderStyle(10, 4);
  if bs <> fbs_None then
  begin
    i := GetBorderColorIndex(12, 7);
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

  end;

  bs := GetBorderStyle(11, 0);
  if bs <> fbs_None then
  begin
    i := GetBorderColorIndex(14, 0);
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

  end;

  bs := GetBorderStyle(11, 4);
  if bs <> fbs_None then
  begin
    i := GetBorderColorIndex(14, 7);
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

  end;

  bs := GetBorderStyleExt(14, 21);
  if bs <> fbs_None then
  begin
    i := GetBorderColorIndexExt(14, 14);
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

  end;

  Pattern := TFlxPatternStyle(CellPattern + 1);
  if Pattern <> 1 then
  begin
    i := CellFgColorIndex;
    if (i >= 0) and (i < Length(UsedColors)) then
      UsedColors[i] := true;

    if Pattern <> 2 then
    begin
      i := CellBgColorIndex;
      if (i >= 0) and (i < Length(UsedColors)) then
        UsedColors[i] := true;

    end;

  end;

  i := FontList[GetActualFontIndex(FontList)].ColorIndex + 1;
  if (i >= 0) and (i < Length(UsedColors)) then
    UsedColors[i] := true;

end;

function TXFRecord.FlxFormat(const FontList: TFontRecordList; const FormatList: TFormatRecordList): TFlxFormat;
var
  FontIdx: integer;
begin
  FontIdx:=FontIndex;
  if FontIdx>=4 then Dec(FontIdx); //Font number 4 does not exists
  if (FontIdx<0) or (FontIdx>=FontList.Count) then FontIdx:=0;
  Result.Font:= FontList[FontIdx].FlxFont;

  Result.Borders.Left.Style:= GetBorderStyle(10,0);
  Result.Borders.Right.Style:= GetBorderStyle(10,4);
  Result.Borders.Top.Style:= GetBorderStyle(11,0);
  Result.Borders.Bottom.Style:= GetBorderStyle(11,4);

  Result.Borders.Left.ColorIndex:= GetBorderColorIndex(12,0);
  Result.Borders.Right.ColorIndex:= GetBorderColorIndex(12,7);
  Result.Borders.Top.ColorIndex:= GetBorderColorIndex(14,0);
  Result.Borders.Bottom.ColorIndex:= GetBorderColorIndex(14,7);

  Result.Borders.Diagonal.Style:=GetBorderStyleExt(14,21);
  Result.Borders.Diagonal.ColorIndex:=GetBorderColorIndexExt(14,14);

  Result.Borders.DiagonalStyle:=DiagonalStyle;

  Result.Format:= FormatList[FormatIndex];

  Result.FillPattern.Pattern:=CellPattern+1;
  Result.FillPattern.FgColorIndex:=CellFgColorIndex;
  Result.FillPattern.BgColorIndex:=CellBgColorIndex;

  Result.HAlignment:=HAlign;
  Result.VAlignment:=VAlign;

  FillMisc(Result.Locked, Result.Hidden, Result.Parent, Result.WrapText,
              Result.ShrinkToFit, Result.Rotation, Result.Indent); //all togheter, to sve some function calls...

end;

function TXFRecord.FontIndex: integer;
begin
  Result:= PXFDat(Data).Font;
end;

function TXFRecord.FormatIndex: integer;
begin
  Result:= PXFDat(Data).Format;
end;

function TXFRecord.GetBorderColorIndex(const aPos: integer; const FirstBit: byte): integer;
begin
  Result:=(GetWord(Data, aPos) shr FirstBit) and $7F -7;
  if Result<1 then Result:=1;
end;

function TXFRecord.GetBorderColorIndexExt(const aPos: integer; const FirstBit: byte): integer;
begin
  Result:=(GetLongWord(Data, aPos) shr FirstBit) and $7F-7;
  if Result<1 then Result:=1;
end;

function TXFRecord.GetBorderStyle(const aPos: integer; const FirstBit: byte): TFlxBorderStyle;
begin
  Result:=TFlxBorderStyle((Data[aPos] shr FirstBit) and $F)
end;

function TXFRecord.GetBorderStyleExt(const aPos: integer; const FirstBit: byte): TFlxBorderStyle;
begin
  Result:=TFlxBorderStyle((GetLongWord(Data, aPos) shr FirstBit) and $F)
end;

function TXFRecord.HAlign: THFlxAlignment;
begin
  Result:=THFlxAlignment(PXFDat(Data).Options6 and $7);
end;

function TXFRecord.VAlign: TVFlxAlignment;
begin
  Result:=TVFlxAlignment(PXFDat(Data).Options6 and $70 shr 4);
end;

function TXFRecord.Rotation: integer;
begin
  Result := Data[7];
end;

function TXFRecord.WrapText: boolean;
begin
  Result := PXFDat(Data).Options6 and $8 = $8;
end;

{ TFontRecord }
function TFontRecord.FlxFont: TFlxFont;
begin
  Result.Name:= Self.Name;
  Result.Size20:= Self.Height;
  Result.ColorIndex:= Self.ColorIndex+1;
  Result.Style:= Self.Style;
  Result.Underline:= Self.Underline;
  Result.Family:=Self.Family;
  Result.CharSet:=Self.CharSet;
end;


function TFontRecord.Charset: byte;
begin
  Result:=Data[12];
end;

function TFontRecord.ColorIndex: integer;
begin
  Result:=GetWord(Data, 4)-8;
end;

constructor TFontRecord.CreateFromFlxFont(const aFont: TFlxFont);
var
  TempData: PArrayOfByte;
  Xs: TExcelString;
begin
  Xs:= TExcelString.Create(1, aFont.Name, true);
  try
    DataSize:=SizeOf(TFontDat) + Xs.TotalSize;
    GetMem(TempData, DataSize);
    try
      PFontDat(TempData).Height:= aFont.Size20;
      PFontDat(TempData).GrBit:= 0;
      if flsItalic in aFont.Style then PFontDat(TempData).GrBit:=PFontDat(TempData).GrBit+2;
      if flsStrikeOut in aFont.Style then PFontDat(TempData).GrBit:=PFontDat(TempData).GrBit+8;

      PFontDat(TempData).ColorIndex:= Word(aFont.ColorIndex+7);
      if flsBold in aFont.Style then PFontDat(TempData).BoldStyle:=$2BC else PFontDat(TempData).BoldStyle:=$190;
      if flsSubscript in aFont.Style then PFontDat(TempData).SuperSub:= 2
      else if flsSuperscript in aFont.Style then PFontDat(TempData).SuperSub:=1
      else PFontDat(TempData).SuperSub:=0;
      case aFont.Underline of
        fu_Single: PFontDat(TempData).Underline:=$01;
        fu_Double: PFontDat(TempData).Underline:=$02;
        fu_SingleAccounting: PFontDat(TempData).Underline:=$21;
        fu_DoubleAccounting: PFontDat(TempData).Underline:=$22;
        else PFontDat(TempData).Underline:=0;
      end; //case

      PFontDat(TempData).Family:=aFont.Family;
      PFontDat(TempData).CharSet:=aFont.CharSet;
      PFontDat(TempData).Reserved:=0;

      Xs.CopyToPtr( TempData, SizeOf(TFontDat) );
      Create( xlr_FONT, TempData, DataSize);
      TempData:=nil;
    finally
      FreeMem(TempData);
    end;
  finally
    FreeAndNil(Xs);
  end;
end;

function TFontRecord.Family: byte;
begin
  Result:=Data[11];
end;

function TFontRecord.Height: integer;
begin
  Result:=GetWord(Data, 0);
end;

function TFontRecord.Name: UTF16String;
var
  w: UTF16String;
  s: AnsiString;
  aPos, DestPos, StrLen: integer;
  OptionFlags, ActualOptionFlags: byte;
  MySelf: TBaseRecord;
begin
  aPos:=16; MySelf:=Self;DestPos:=0;
  StrLen:=Data[14];
  OptionFlags:=Data[15]; ActualOptionFlags:=OptionFlags;
  SetLength(s, StrLen);
  SetLength(w, StrLen);
  ReadStr( MySelf, aPos, s, w, OptionFlags, ActualOptionFlags, DestPos, StrLen );
  if (OptionFlags and $1) = 0 then Result:=StringToWideStringNoCodePage(s) else Result:=w;

end;

function TFontRecord.Style: SetOfTFlxFontStyle;
begin
  Result:=[];
  if GetWord(Data,6)=$2BC then Include(Result,flsBold);
  if GetWord(Data,2) and $02=$02 then Include(Result,flsItalic);
  if GetWord(Data,2) and $08=$08 then Include(Result,flsStrikeOut);
  case GetWord(Data,8) of
    1: Include(Result,flsSuperscript);
    2: Include(Result,flsSubscript);
  end; //case
end;

function TFontRecord.Underline: TFlxUnderline;
begin
  case data[10] of
    $01: Result:=fu_Single;
    $02: Result:= fu_Double;
    $21: Result:=fu_SingleAccounting;
    $22: Result:=fu_DoubleAccounting;
    else Result:=fu_None;
  end;//case
end;

{ TFormatRecord }

constructor TFormatRecord.CreateFromData(const Fmt: UTF16String; const NewID: integer);
var
  TempData: PArrayOfByte;
  Xs: TExcelString;
begin
  Xs:= TExcelString.Create(2, Fmt);
  try
    DataSize:=2 + Xs.TotalSize;
    GetMem(TempData, DataSize);
    try
      SetWord( TempData, 0, NewID);
      Xs.CopyToPtr( TempData, 2 );
      Create( xlr_Format, TempData, DataSize);
      TempData:=nil;
    finally
      FreeMem(TempData);
    end;
  finally
    FreeAndNil(Xs);
  end;
end;

function TFormatRecord.FormatId: integer;
begin
  Result:=GetWord(Data, 0);
end;

function TFormatRecord.Value: UTF16String;
var
  MySelf: TBaseRecord;
  aPos: integer;
  StrLen: integer;
  s: AnsiString;
  w: UTF16String;
  OptionFlags, ActualOptionFlags: byte;
  DestPos: integer;
begin
  aPos:=5;MySelf:=Self;DestPos:=0;
  OptionFlags:=Data[4]; ActualOptionFlags:=OptionFlags;
  StrLen:=GetWord(Data, 2);
  SetLength(s, StrLen);
  SetLength(w, StrLen);
  ReadStr( MySelf, aPos, s, w, OptionFlags, ActualOptionFlags, DestPos, StrLen );
  if (OptionFlags and $1) = 0 then Result:=StringToWideStringNoCodePage(s) else Result:=w;
end;

{ TFormatRecordList }

function TFormatRecordList.AddFormat(const Fmt: UTF16String): integer;
var
  i: integer;
  NewId: integer;
begin
  for i:=0 to Count-1 do
    if Fmt= Items[i].Value then
    begin
      Result:=Items[i].FormatId;
      exit;
    end;

    for i:= Low(XlsBuiltInFormatsUs) to High(XlsBuiltInFormatsUs) do
      if Fmt=XlsBuiltInFormats(i) then
      begin
        Result:=i;
        exit;
      end;

  if not Sorted then Sort;
  if Count=0 then  NewId:=$A4 else NewId:= Items[Count-1].FormatId+1;
  if NewId<$A4 then NewId:=$A4;  //user defined format.
  Result:=Items[Add(TFormatRecord.CreateFromData(Fmt, NewId))].FormatId;
end;

function TFormatRecordList.GetFormat(FormatId: integer): UTF16String;
var
  Index: integer;
begin
  if Find(FormatId, Index) then Result:=Items[Index].Value else
  if (FormatId>=Low(XlsBuiltInFormatsUs)) and (FormatId<=High(XlsBuiltInFormatsUs)) then Result:=XlsBuiltInFormats(FormatId)
  else Result:='';
end;

{ TXFRecordList }

function TXFRecordList.FindFormat(const XF: TXFRecord; out Index: integer): boolean;
var
  i: integer;
begin
  for i:=0 to Count -1 do if
    CompareMem(Items[i].Data, XF.Data, XF.DataSize) then
    begin
      Result:=true;
      Index:=i;
      exit;
    end;
  Result:=false;
  Index:=-1;
end;

function TXFRecordList.GetUsedColors(const ColorCount: integer; const FontList: TFontRecordList): BooleanArray;
var
  i: integer;
begin
  SetLength (Result, ColorCount);
  if (ColorCount > 0) then FillChar(Result[0], Length(Result), 0);
  for i := 0 to Count - 1 do
        Self[i].FillUsedColors(Result, FontList);
end;

{ TFontRecordList }

function TFontRecordList.AddFont(const aFont: TFlxFont): integer;
var
  i: integer;
  TempFont: TFontRecord;
begin
  Result:=-1;
  TempFont:= TFontRecord.CreateFromFlxFont(aFont);
  try
    for i:=0 to Count-1 do if (TempFont.DataSize=Items[i].DataSize) and
      CompareMem(TempFont.Data, Items[i].Data, Items[i].DataSize) then
      begin
        Result:=i; if Result>=4 then Inc(Result); //Font number 4 does not exists
        FreeAndNil(TempFont);
        exit;
      end;
    Result:=Add(TempFont);
    if Result>=4 then Inc(Result); //Font number 4 does not exists
  except
    FreeAndNil(TempFont);
  end; //except
end;

end.
