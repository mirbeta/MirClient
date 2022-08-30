/// Miscellaneous utilities
unit tmsUFlxUtils;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUExcelAdapter, tmsUFlxMessages, SysUtils, tmsXlsMessages
{$IFNDEF FIREMONKEY}
  ,Graphics
{$ELSE}
  ,FMX.Types, System.UITypes
{$ENDIF}
;
//------------------------------------------------------------------------//

  /// <summary>
  /// \Returns the most similar entry on the excel palette for a given color.
  /// </summary>
  /// <remarks>
  /// <b>You will normally want to use <see cref="TFlexCelImport.NearestColorIndex@TColor@BooleanArray" text="TFlexCelImport.NearestColorIndex" />
  /// instead of this method.</b><para></para>
  /// <para></para>
  /// If UsedColors is not nil, it will try to modify the Excel color palette to get a better match on the
  /// color, modifying among the not used colors. Note that modifying the standard palette might result on
  /// a file that is not easy to edit on Excel later, since it does not have the standard Excel colors.
  /// </remarks>
  /// <param name="Workbook">Excel file where you want to get the color.</param>
  /// <param name="aColor">Color for which you want to find the nearest one.</param>
  /// <param name="UsedColors">A list of used colors.</param>
  /// <returns>
  /// The color that is nearest to the one specified in aColor.
  /// </returns>                                                                                                                                  
  function MatchNearestColor(const Workbook: TExcelFile; const aColor: TColor;
                             const UsedColors: BooleanArray): integer;

  /// <summary>
  /// Calculates the needed parameters to place an image in Excel given its dimensions in pixels.
  /// </summary>
  /// <remarks>
  /// Normally you don't need to call this method directly, since TFlexCelImport has overload for the
  /// methods that accept image dimensions in pixels and internally use this method to enter the image in
  /// Excel.<para></para>
  /// Use CalcImgDimensions to make the inverse convert, from Excel units to pixels.
  /// </remarks>
  /// <param name="Workbook">Workbook where you are going to place the image. FlexCel needs this
  ///                        value so it can know the height of rows and width of columns, so it
  ///                        can calculate how many rows and columns the image needs.</param>
  /// <param name="Row">Row where you are going to place the image.</param>
  /// <param name="Col">Column where you are going to place the image.</param>
  /// <param name="dh">Offset from the top of the row for the image. (1/255 of a cell. 0
  ///                  means totally at the top, 128 on half of the cell, 255 means at the
  ///                  top of next cell.)</param>
  /// <param name="dw">Offset from the left of the cell for the image. (1/255 of a cell. 0
  ///                  means totally at the top, 128 on half of the cell, 255 means at the
  ///                  top of next cell.)</param>
  /// <param name="ImgHeightInPixels">Height you want for the image to have in pixels.</param>
  /// <param name="ImgWidthInPixels">Width you want for the image to have in pixels.</param>
  /// <param name="Props">\Returns the column and rows where the image will be placed.</param>
  procedure CalcImgCells(const Workbook: TExcelFile; const Row, Col, dh, dw:integer; const ImgHeightInPixels, ImgWidthInPixels: extended; out Props: TImageProperties);

  /// <summary>
  /// Calculates the image dimensions in pixels given an Excel anchor.
  /// </summary>
  /// <remarks>
  /// This method is the reverse of CalcImgCells, and it gives you a way to calculate the width and height
  /// of an image in pixels.<para></para>
  /// <para></para>
  /// There are two very similar overloads for this method, one returns the pixels as integers, the other
  /// as floating numbers. Even when pixels are integer numbers, the floating point version might be good
  /// if you are manipulating the magnitudes (for example adding them), to avoid rounding errors. You can
  /// round to pixels at the end of the operations, not on each step.
  /// </remarks>
  /// <param name="Workbook">\File where the image is located.</param>
  /// <param name="Anchor">Excel anchor indicating the columns and rows where the image is located.</param>
  /// <param name="w">\Returns the width in pixels for the image</param>
  /// <param name="h">\Returns the height in pixels from the image.</param>                                
  procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out w, h: integer);overload;

  /// <summary>
  /// Calculates the image dimensions in pixels given an Excel anchor.
  /// </summary>
  /// <remarks>
  /// This method is the reverse of CalcImgCells, and it gives you a way to calculate the width and height
  /// of an image in pixels.<para></para>
  /// <para></para>
  /// There are two very similar overloads for this method, one returns the pixels as integers, the other
  /// as floating numbers. Even when pixels are integer numbers, the floating point version might be good
  /// if you are manipulating the magnitudes (for example adding them), to avoid rounding errors. You can
  /// round to pixels at the end of the operations, not on each step.
  /// </remarks>
  /// <param name="Workbook">\File where the image is located.</param>
  /// <param name="Anchor">Excel anchor indicating the columns and rows where the image is located.</param>
  /// <param name="w">\Returns the width in pixels for the image</param>
  /// <param name="h">\Returns the height in pixels from the image.</param>                                
  procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out w, h: extended); overload;

  /// <summary>
  /// Calculates the image dimensions in pixels given an Excel anchor.
  /// </summary>
  /// <remarks>
  /// This method is the reverse of CalcImgCells, and it gives you a way to calculate the width and height
  /// of an image in pixels.
  /// </remarks>
  /// <param name="Workbook">\File where the image is located.</param>
  /// <param name="Anchor">Excel anchor indicating the columns and rows where the image is located.</param>
  /// <param name="XOfsPixel">\Returns how many pixels the image is offset from the left of the cell.</param>
  /// <param name="YOfsPixel">\Returns how many pixels the image is offset from the top of the cell.</param>
  /// <param name="w">\Returns the width in pixels for the image</param>
  /// <param name="h">\Returns the height in pixels from the image.</param>
  procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out XOfsPixel, YOfsPixel, w, h: extended); overload;
//-------------------------------------------------------------------------//
implementation

{$IFDEF FIREMONKEY}
function BgrToRgb(const aColor: Int64): Int32;
begin
  Result := Int32((((aColor and $FF00) or ((aColor and $FF0000) shr $10)) or ((aColor and $FF) shl $10)));
end;

function ColorToRgb(const aColor: TColor): TColor;
begin
  if Assigned(TColors.ColorToRGB) then begin; Result := UInt32(BgrToRgb(TColors.ColorToRGB(aColor))); exit; end;
  if (aColor and $FF000000) <> 0 then exit(TColorRec.White); //System color, and we don't have a ColorToRGB

  Result := aColor;

end;
{$ENDIF}
function MatchNearestColor(const Workbook: TExcelFile; const aColor: TColor;
  const UsedColors: BooleanArray): integer;
type
  TCb= array[0..3] of byte;
var
  i: integer;
  sq, MinSq: extended;
  ac1, ac2: TCb;
  Result2: integer;
begin
  Result:=1;
  MinSq:=-1;
  ac1 := TCb(ColorToRgb(aColor));
  for i:=1 to 55 do
  begin
    ac2:=TCb(Workbook.ColorPalette[i]);
    sq := Sqr(ac2[0] - ac1[0]) +
          Sqr(ac2[1] - ac1[1]) +
          Sqr(ac2[2] - ac1[2]);
    if (MinSq<0) or (sq< MinSq) then
    begin
      MinSq:=sq;
      Result:=i;

      if sq=0 then
      begin
        if (UsedColors <> nil) then UsedColors[Result] := true;
        exit; //exact match...
      end
    end;
  end;

  if (UsedColors = nil) then exit;

  //Find the nearest color between the ones that are not in use.
  UsedColors[0] := true; //not really used
  UsedColors[1] := true; //pure black
  UsedColors[2] := true; //pure white

  Result2:=-1;
  MinSq:=-1;
  for i:=1 to 55 do
  begin
    if (Length(UsedColors) <= i) or UsedColors[i] then continue;

    ac2:=TCb(Workbook.ColorPalette[i]);
    sq := Sqr(ac2[0] - ac1[0]) +
          Sqr(ac2[1] - ac1[1]) +
          Sqr(ac2[2] - ac1[2]);
    if (MinSq<0) or (sq< MinSq) then
    begin
      MinSq:=sq;
      Result2:=i;
      if sq=0 then
      begin
        Result := Result2;
        if (UsedColors <> nil) then UsedColors[Result] := true;
        exit; //exact match...
      end;
    end;
  end;

  if (Result2 < 0) or (Result2 >= Length(UsedColors)) then
  begin
    if (UsedColors <> nil) then UsedColors[Result] := true;
    exit;  //Not available colors to modify
  end;
  Workbook.ColorPalette[Result2] := ColorToRGB(aColor);
  UsedColors[Result2] := true;
  Result:= Result2;
end;

//----------------------------------------------------------------------
procedure CalcImgCells(const Workbook: TExcelFile; const Row, Col, dh, dw:integer; const ImgHeightInPixels, ImgWidthInPixels: extended; out Props: TImageProperties);

  function Rh(const Row: integer): extended;
  begin
    if not Workbook.IsEmptyRow(Row) then Result:=Workbook.RowHeightHiddenIsZero[Row]/RowMult else
      Result:=Workbook.DefaultRowHeight/RowMult;
  end;

  function Cw(const Col: integer): extended;
  begin
    Result:=Workbook.ColumnWidthHiddenIsZero[Col]/ColMult;
  end;

var
  r, c : integer;
  h, w: extended;
  Row1, Col1: integer;
  dx1, dy1: extended;
  EmptyProps: TImageProperties;
begin
  if Workbook=nil then raise Exception.Create(ErrNoOpenFile);

  FillChar(EmptyProps, SizeOf(EmptyProps), 0); //Just to make sure all record is empty. We can fillchar because strings are initialized to nil in local variables.
  Props := EmptyProps;

  Row1:=Row; Col1:=Col; dx1:=dw; dy1:=dh;
  //If delta spawns more than one cell, advance the cells.
  while dx1>Cw(Col1) do
  begin
    dx1:=dx1- Cw(Col1);
    inc(Col1);
  end;
  while dy1>Rh(Row1) do
  begin
    dy1:=dy1- Rh(Row1);
    inc(Row1);
  end;

  if Row1<1 then begin Row1:=1;dy1:=0;end;
  if Col1<1 then begin Col1:=1;dx1:=0;end;

  Props.Row1:=Row1;
  Props.Col1:=Col1;
  Props.dx1:=Round(1024*dx1/Cw(Col1));
  Props.dy1:=Round(255*dy1/Rh(Row1));

  r:=Row1; h:=Rh(Row1)-dy1;
  while Round(h)<ImgHeightInPixels do
  begin
    inc(r);
    h:=h+ Rh(r);
  end;
  Props.Row2:=r;
  Props.dy2:=Round((Rh(r)-(h-ImgHeightInPixels))/Rh(r)*255);

  c:=Col1; w:=Cw(Col1)-dx1;
  while Round(w)<ImgWidthInPixels do
  begin
    inc(c);
    w:=w+Cw(c);
  end;
  Props.Col2:=c;
  Props.dx2:=Round((Cw(c)-(w-ImgWidthInPixels))/Cw(c)*1024);

  if Props.Row2>Max_Rows+1 then
  begin
    Props.Row1:=Max_Rows+1-(Props.Row2-Props.Row1);
    Props.Row2:=Max_Rows+1;
  end;
  if Props.Col2>Max_Columns+1 then
  begin
    Props.Col1:=Max_Columns+1-(Props.Col2-Props.Col1);
    Props.Col2:=Max_Columns+1;
  end;
  //Just in case of an image bigger than the spreadsheet...
  if Props.Col1<1 then Props.Col1:=1;
  if Props.Row1<1 then Props.Row1:=1;
end;

procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out XOfsPixel, YOfsPixel, w, h: extended);overload;

  function Rh(const Row: integer): extended;
  begin
    if not Workbook.IsEmptyRow(Row) then Result:=Workbook.RowHeightHiddenIsZero[Row]/RowMult else
      Result:=Workbook.DefaultRowHeight/RowMult;
  end;

  function Cw(const Col: integer): extended;
  begin
    Result:=Workbook.ColumnWidthHiddenIsZero[Col]/ColMult;
  end;

var
  i: integer;
begin
  w:=0;
  for i:=Anchor.Col1 to Anchor.Col2-1 do w:=w+ Cw(i);
  XOfsPixel := (Cw(Anchor.Col1)*(Anchor.Dx1)/1024);
  w:=w - XOfsPixel;
  w:=w +(Cw(Anchor.Col2)*(Anchor.Dx2)/1024);

  h:=0;
  for i:=Anchor.Row1 to Anchor.Row2-1 do h:=h+ Rh(i);
  YOfsPixel := (Rh(Anchor.Row1)*(Anchor.Dy1)/255);
  h:=h - YOfsPixel;
  h:=h + (Rh(Anchor.Row2)*(Anchor.Dy2)/255);
end;

procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out w, h: integer);overload;
var
  w1, h1: extended;
begin
  CalcImgDimentions(Workbook,Anchor,w1, h1);
  w:=Round(w1);
  h:=Round(h1);
end;

procedure CalcImgDimentions(const Workbook: TExcelFile; const Anchor: TClientAnchor; out w, h: extended);overload;
var
  w1, h1: extended;
  XOfsPixel, YOfsPixel: extended;
begin
  CalcImgDimentions(Workbook,Anchor, XOfsPixel, YOfsPixel, w1, h1);
  w:=Round(w1);
  h:=Round(h1);
end;
end.
