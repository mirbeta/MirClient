/// Utilities to load or save text delimited files.
unit tmsUTextDelim;
{$INCLUDE ..\FLXCOMPILER.INC}
interface
  uses Classes, SysUtils, tmsUExcelAdapter, tmsUFlxNumberFormat, tmsUFlxMessages;

type

  /// <summary>
  /// Handles how to convert a column from text when importing a text file.
  /// </summary>
  XLSColumnImportTypes=(
    /// <summary>
    /// Try to convert it to a number, a date, etc.
    /// </summary>
    xct_general,

    /// <summary>
    /// Keep the column as text, even if it can be converted to a number or other things.
    /// </summary>
    xct_text,

		/// <summary>
    /// Do not import this column.
    /// </summary>
    xct_skip);

//***********************************************************************************

      /// <summary>
      /// Saves the Active sheet of an Excel file as a text delimited file.
      /// </summary>
      /// <remarks>
      /// You will normally want to use <see cref="TFlexCelImport.SaveAsText@TFileName@Char" text="TFlexCelImport.SaveAsText" />
      /// instead of this method to save whole files, or SaveRangeAsTextDelim to save a range of the active
      /// sheet.
      /// </remarks>
      /// <param name="OutStream">Stream where we are going to save the file.</param>
      /// <param name="Workbook">Workbook we want to save.</param>
      /// <param name="Delim">Delimiter used in the file (&quot;,&quot; or &quot;;&quot; for csv, #9
      ///                     for tab delimited)</param>                                                                             
      procedure SaveAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char); {$IFDEF DELPHI2008UP}overload;{$ENDIF}

      /// <summary>
      /// Saves a range of cells in a text delimited file.
      /// </summary>
      /// <remarks>
      /// To save the full sheet use SaveAsTextDelim or <see cref="TFlexCelImport.SaveAsText@TFileName@Char" text="TFlexCelImport.SaveAsText" />.
      /// </remarks>
      /// <param name="OutStream">Stream where we are going to save the file.</param>
      /// <param name="Workbook">Workbook we want to save.</param>
      /// <param name="Delim">Delimiter used in the file (&quot;,&quot; or &quot;;&quot; for csv, #9
      ///                     for tab delimited)</param>
      /// <param name="Range">Encoding for the generated file.</param>
      procedure SaveRangeAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char; const Range: TXlsCellRange); {$IFDEF DELPHI2008UP}overload;{$ENDIF}

      /// <summary>
      /// Imports a text file into an Excel file.
      /// </summary>
      /// <remarks>
      /// Normally you can just use <see cref="TFlexCelImport.OpenText@TFileName@Char@array of XLSColumnImportTypes" text="TFlexCelImport.OpenFile Method" />
      /// to import csv files, but this method gives you a little more control, like the row and column where
      /// to import the data.<para></para>
      /// Actually OpenText internally calls this method.
      /// </remarks>
      /// <param name="InStream">Stream with the text delimited file you want to import.</param>
      /// <param name="Workbook">Excel file where there data will be imported.</param>
      /// <param name="aDelim">Delimiter used in the file. This is normally &quot;,&quot;, &quot;;&quot;
      ///                      in csv files or #9 (tab) in tab delimited files.</param>
      /// <param name="FirstRow">First row where the data will be imported. (1 based)</param>
      /// <param name="FirstCol">First column where the data will be imported (1 based)</param>
      /// <param name="ColumnFormats">Array of import types specifying how to import each column.</param>
      procedure LoadFromTextDelim(const InStream: TStream; const Workbook: TExcelFile; const aDelim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes); {$IFDEF DELPHI2008UP}overload;{$ENDIF}

{$IFDEF DELPHI2008UP}
      /// <summary>
      /// Saves the Active sheet of an Excel file as a text delimited file.
      /// </summary>
      /// <remarks>
      /// You will normally want to use <see cref="TFlexCelImport.SaveAsText@TFileName@Char" text="TFlexCelImport.SaveAsText" />
      /// instead of this method to save whole files, or SaveRangeAsTextDelim to save a range of the active
      /// sheet.<para></para>
      /// <para></para>
      /// <b>This method only works in Delphi 2009 or newer.</b>
      /// </remarks>
      /// <param name="OutStream">Stream where we are going to save the file.</param>
      /// <param name="Workbook">Workbook we want to save.</param>
      /// <param name="Delim">Delimiter used in the file (&quot;,&quot; or &quot;;&quot; for csv, #9
      ///                     for tab delimited)</param>
      /// <param name="Encoding">Encoding for the saved file.</param>
      procedure SaveAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char; const Encoding: TEncoding); overload;

      /// <summary>
      /// Saves a range of cells in a text delimited file.
      /// </summary>
      /// <remarks>
      /// To save the full sheet use SaveAsTextDelim or <see cref="TFlexCelImport.SaveAsText@TFileName@Char" text="TFlexCelImport.SaveAsText" />.<para></para>
      /// <para></para>
      /// <b>This method only works in Delphi 2009 or newer.</b>
      /// </remarks>
      /// <param name="OutStream">Stream where we are going to save the file.</param>
      /// <param name="Workbook">Workbook we want to save.</param>
      /// <param name="Delim">Delimiter used in the file (&quot;,&quot; or &quot;;&quot; for csv, #9
      ///                     for tab delimited)</param>
      /// <param name="Range">Range of cells we want to export.</param>
      /// <param name="Encoding">Encoding for the generated file.</param>                                                                                          
      procedure SaveRangeAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char; const Range: TXlsCellRange; const Encoding: TEncoding); overload;

      /// <summary>
      /// Imports a text file into an Excel file.
      /// </summary>
      /// <remarks>
      /// Normally you can just use <see cref="TFlexCelImport.OpenText@TFileName@Char@array of XLSColumnImportTypes" text="TFlexCelImport.OpenFile Method" />
      /// to import csv files, but this method gives you a little more control, like the row and column where
      /// to import the data.<para></para>
      /// <para></para>
      /// <b>This overload of the method is only available in Delphi 2009 or newer.</b>
      /// </remarks>
      /// <param name="InStream">Stream with the text delimited file you want to import.</param>
      /// <param name="Workbook">Excel file where there data will be imported.</param>
      /// <param name="Delim">Delimiter used in the file. This is normally &quot;,&quot;, &quot;;&quot;
      ///                     in csv files or #9 (tab) in tab delimited files.</param>
      /// <param name="FirstRow">First row where the data will be imported. (1 based)</param>
      /// <param name="FirstCol">First column where the data will be imported (1 based)</param>
      /// <param name="ColumnFormats">Array of import types specifying how to import each column.</param>
      /// <param name="Encoding">Encoding used in the text file. (UTF8, Unicode, etc).</param>
      /// <param name="DetectBOM">If true, FlexCel will try to detect the encoding from the BOM (Byte
      ///                         order mark) in the file. Set it to true if the files have BOM.</param>
      procedure LoadFromTextDelim(const InStream: TStream; const Workbook: TExcelFile; const Delim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes;
                   const Encoding: TEncoding; const DetectBOM: Boolean = false); overload;

      /// <summary>
      /// Imports a text file into an Excel file.
      /// </summary>
      /// <remarks>
      /// Normally you can just use <see cref="TFlexCelImport.OpenText@TFileName@Char@array of XLSColumnImportTypes" text="TFlexCelImport.OpenFile Method" />
      /// to import csv files, but this method gives you a little more control, like the row and column where
      /// to import the data.<para></para>
      /// <para></para>
      /// <b>This overload of the method is only available in Delphi 2009 or newer.</b>
      /// </remarks>
      /// <param name="Sr">StreamReader with the text delimited file you want to import.</param>
      /// <param name="Workbook">Excel file where there data will be imported.</param>
      /// <param name="Delim">Delimiter used in the file. This is normally &quot;,&quot;, &quot;;&quot;
      ///                     in csv files or #9 (tab) in tab delimited files.</param>
      /// <param name="FirstRow">First row where the data will be imported. (1 based)</param>
      /// <param name="FirstCol">First column where the data will be imported (1 based)</param>
      /// <param name="ColumnFormats">Array of import types specifying how to import each column.</param>
      procedure LoadFromTextDelim(const Sr: TStreamReader; const Workbook: TExcelFile; const Delim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes); overload;
{$ENDIF}


//***********************************************************************************
implementation

function FlxQuotedStr(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '"' then Insert('"', Result, I);
  Result := '"' + Result + '"';
end;


procedure SaveRangeAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char; const Range: TXlsCellRange
                               {$IFDEF DELPHI2008UP}; const Encoding: TEncoding {$ENDIF});
var
  r,c: integer;
  s: String;  //UTF16 in D2009, AnsiString otherwise.
  Color: integer;
  {$IFDEF DELPHI2008UP}
  Buff : TBytes;
  {$ENDIF}
begin
  for r:=Range.Top to Range.Bottom do
  begin
    for c:=Range.Left to Range.Right do
    begin
      Color := -1;
      s:=XlsFormatValue1904(Workbook.CellValue[r,c],Workbook.FormatList[Workbook.CellFormat[r,c]].Format, Workbook.Options1904Dates, Color);
      if (pos(Delim, s)>0) or (pos('"', s)>0) or (pos(#10,s)>0) or (pos(#13,s)>0) then
      begin
        s:=FlxQuotedStr(s);
      end;
      if c<Range.Right then s:=s+Delim else s:=s+#13#10;

{$IFDEF DELPHI2008UP}
      Buff := Encoding.GetBytes(s);
      OutStream.Write(Buff[0], Length(Buff));
{$ELSE}
      OutStream.Write(s[1], Length(s));
{$ENDIF}
    end;
  end;
end;


procedure SaveAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char
                          {$IFDEF DELPHI2008UP}; const Encoding: TEncoding {$ENDIF});

var
  Range:TXlsCellRange;
begin
  Range.Left:=1;
  Range.Top:=1;
  Range.Right:=Workbook.MaxCol;
  Range.Bottom:=Workbook.MaxRow;
{$IFDEF DELPHI2008UP}
  SaveRangeAsTextDelim(OutStream, Workbook, Delim, Range, Encoding);
{$ELSE}
  SaveRangeAsTextDelim(OutStream, Workbook, Delim, Range);
{$ENDIF}
end;


{$IFDEF DELPHI2008UP}
procedure ReadQString(const InStream: TStreamReader; var S: TStringBuilder; var ch: integer);
var
  InQuote: boolean;
begin
  InQuote:=false;
  S.Length := 0;
  while ch >= 0 do
  begin
    ch := InStream.Read;
    if (ch<> Ord('"')) and InQuote then
    begin
      exit;
    end;
    if InQuote or (ch<> Ord('"')) then s.Append(char(ch));
    InQuote:=(ch= Ord('"')) and not InQuote;
  end;
end;

procedure ReadNString(const InStream: TStreamReader; const Delim: Integer; var S: TStringBuilder; var ch: integer);
begin
  s.Length := 0;
  s.Append(char(ch));
  while Ch >= 0 do
  begin
    ch := InStream.Read;
    if (ch=Delim)or (ch=13)or (ch=10) or (ch < 0)  then exit;
    s.Append(char(ch));
  end; //while
end;

procedure LoadFromTextDelim(const InStream: TStream; const Workbook: TExcelFile; const Delim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes;
                          const Encoding: TEncoding; const DetectBOM: Boolean = false);
var
  Sr: TStreamReader;
  Enc: TEncoding;
begin
  Enc := Encoding;
  if Enc = nil then Enc := TEncoding.ASCII;

  Sr := TStreamReader.Create(InStream, Enc, DetectBOM); //OwnsStream is false, so it won't free the stream.
  try
    LoadFromTextDelim(Sr, Workbook, Delim, FirstRow, FirstCol, ColumnFormats);
  finally
    FreeAndNil(Sr);
  end;
end;

procedure LoadFromTextDelim(const Sr: TStreamReader; const Workbook: TExcelFile; const Delim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes);

var
  r,c: integer;
  s: TStringBuilder;
  ch: integer;
  bDelim : integer;
begin
  bDelim := ord(Delim);
  r:=FirstRow;
  c:=FirstCol;

  s := TStringBuilder.Create;
  try
    ch := Sr.Read;
    while ch >= 0 do
    begin
      if (ch= Ord('"')) then ReadQString(Sr, s, ch)
      else if (ch=bDelim) then
      begin
        inc(c);
        Ch := Sr.Read;
        continue;
      end
      else if (ch=10) then
      begin
        c:=FirstCol;
        inc(r);
        ch := Sr.Read;
        continue;
      end else if (ch=13) then
      begin
        ch := Sr.Read;
        continue;
      end
      else ReadNString(Sr, bDelim, s, ch);

      if c-FirstCol< Length(ColumnFormats) then
        case ColumnFormats[c-FirstCol] of
          xct_text: Workbook.CellValue[r, c]:=s.ToString;
          xct_skip: begin end;
          else WorkBook.SetCellString(r,c,s.ToString);
        end //case
      else WorkBook.SetCellString(r,c,s.ToString);
    end;
  finally
    FreeAndNil(s);
  end;
end;

{$ELSE}
procedure ReadQString(const InStream: TStream; out S: String; var ch: Char);
var
  InQuote: boolean;
begin
  InQuote:=false;
  s:='';
  while InStream.Position<InStream.Size do
  begin
    InStream.ReadBuffer(ch, SizeOf(ch));
    if (ch<>'"') and InQuote then
    begin
      exit;
    end;
    if InQuote or (ch<>'"') then s:=s+ch;
    InQuote:=(ch='"') and not InQuote;
  end;
end;

procedure ReadNString(const InStream: TStream; const Delim: Char; var S: String; var ch: Char);
begin
  s:=ch;
  while InStream.Position<InStream.Size do
  begin
    InStream.ReadBuffer(ch, SizeOf(ch));
    if (ch=Delim)or (ch=#13)or (ch=#10)  then exit;
    s:=s+ch;
  end; //while
end;

procedure LoadFromTextDelim(const InStream: TStream; const Workbook: TExcelFile; const aDelim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes);
var
  r,c: integer;
  s: String;
  ch: Char;
  Delim: Char;
begin
  Delim := Char(aDelim);
  r:=FirstRow;
  c:=FirstCol;
  if InStream.Position<InStream.Size then InStream.ReadBuffer(ch, SizeOf(ch));
  while InStream.Position<InStream.Size do
  begin
    if (ch='"') then ReadQString(InStream, s, ch)
    else if (ch=Delim) then
    begin
      inc(c);
      InStream.ReadBuffer(ch, SizeOf(ch));
      continue;
    end
    else if (ch=#10) then
    begin
      c:=FirstCol;
      inc(r);
      InStream.ReadBuffer(ch, SizeOf(ch));
      continue;
    end else if (ch=#13) then
    begin
      InStream.ReadBuffer(ch, SizeOf(ch));
      continue;
    end
    else ReadNString(InStream, Delim, s, ch);

    if c-FirstCol< Length(ColumnFormats) then
      case ColumnFormats[c-FirstCol] of
        xct_text: Workbook.CellValue[r, c]:=s;
        xct_skip: begin end;
        else WorkBook.SetCellString(r,c,s);
      end //case
    else WorkBook.SetCellString(r,c,s);
  end;
end;

{$ENDIF}

{$IFDEF DELPHI2008UP}
procedure SaveAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char);
begin
  SaveAsTextDelim(OutStream, Workbook, Delim, TEncoding.ASCII);
end;

procedure SaveRangeAsTextDelim(const OutStream: TStream; const Workbook: TExcelFile; const Delim: Char; const Range: TXlsCellRange);
begin
  SaveRangeAsTextDelim(OutStream, Workbook, Delim, Range, TEncoding.ASCII);
end;

procedure LoadFromTextDelim(const InStream: TStream; const Workbook: TExcelFile; const aDelim: Char; const FirstRow, FirstCol: integer; const ColumnFormats: array of XLSColumnImportTypes);
begin
  LoadFromTextDelim(InStream, Workbook, aDelim, FirstRow, FirstCol, ColumnFormats, nil);
end;
{$ENDIF}


end.
