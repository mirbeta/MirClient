unit CustomDrawDemoUtils;

interface

uses Windows, Graphics, cxGraphics, cxVGrid;

type
  TcxItemCustomDrawType = (itNormal, itText, itCell);
  TCustomDrawingStyle = (cdsBkImage, cdsGradient, cdsDefaultDrawing, cdsDependsOnData);
  TCustomDrawArea = (cdaBackground, cdaCategory, cdaCell, cdaHeader);
  TColorScheme = (csGrey, csGold, csBlue, csGreen);

  TBkImage = (bkiTile, bkiSky, bkiEgypt, bkiMyFace, bkiUserDefined);
  TColorSchemeArr = array [0..3, 0..2] of TColor;
  TArrRect = array of TRect;
  TLineInfo = (liTop, liBottom);
  TLineInfos = set of TLineInfo;

const
  clBlueDark = TColor($00C56A31);
  clBlueLight = TColor($00F7EAD9);
  clBlueBright = TColor($00FF953D);
  clBlueSky = TColor($00EBC4A4);

  clGold = TColor($0047D5FE);
  clGoldDark = TColor($0001BDF3);

  clGreyLight = TColor($00E2EFF1);
  clGreyDark = TColor($00B9D9DD);
  clYellowLight = TColor($00E1FFFF);

  clGreenBright = TColor($0082E887);
  clGreenLight = TColor($00C9F5CB);
  clGreenObscured = TColor($00ACF0AF);
  clGreenDark = TColor($0044DD4B);
  clSilverDark = TColor($00A6A6A6);

  ColorScheme : TColorSchemeArr  = ((clSilver, clWhite, clGray),(clGold, clGreyLight, clGoldDark),(clBlueDark, clBlueLight, clBlueDark),(clGreenDark, clGreenLight, clGreen));
  BkImageResNames: array [TBkImage] of string = ('TILE', 'SKY', 'EGYPT', 'MYFACE', 'CAR');
  ColorSchemeNames: array [TColorScheme] of string = ('Grey', 'Gold', 'Blue', 'Green');

  CustomDrawAreaNames: array [TCustomDrawArea] of string = ('Background',
    'Category', 'Cell', 'Header');

procedure LoadImageFromRes(ABitmap: TBitMap; AResName: String);
function GetIndents(AHeaderViewInfo: TcxCustomRowHeaderInfo; ALineInfos: TLineInfos): TArrRect;
procedure FillRects(ACanvas: TcxCanvas; AColor: TColor; ARects: TArrRect);

implementation

uses SysUtils, Classes, Dialogs, cxVGridUtils, cxGeometry, StdCtrls;

procedure FillRects(ACanvas: TcxCanvas; AColor: TColor; ARects: TArrRect);
var
  i: Integer;
begin
  ACanvas.Brush.Color := AColor;
  for i:=0 to High(ARects) do
    ACanvas.FillRect(ARects[i]);
end;

procedure LoadImageFromRes(ABitmap: TBitMap; AResName: String);
var
  Rs: TResourceStream;
  BitMap: TBitMap;
begin
  BitMap := TBitMap.Create;
  Rs := TResourceStream.Create(hInstance,
                               AResName, RT_RCDATA);
  try
    BitMap.LoadFromStream(Rs);
    ABitMap.Assign(BitMap);
  finally
    BitMap.Free;
    Rs.Free;
  end;
end;

function GetIndents(AHeaderViewInfo: TcxCustomRowHeaderInfo; ALineInfos: TLineInfos): TArrRect;
var
  ABorders: TcxBorders;
  ARow: TcxCustomRow;
  IsBottomNeeded, IsTopNeeded: Boolean;
  I: Integer;

  procedure AddIndentRect(ARect: TRect; ABorders: TcxBorders; var ARects: TArrRect); 
  begin
    if liTop in ALineInfos then
      ABorders := ABorders + [bTop];
    if liBottom in ALineInfos then
      ABorders := ABorders + [bBottom];
    with ARect do
    begin
      if bLeft in ABorders then
      begin
        SetLength(Result, Length(Result) + 1);
        ARects[High(Result)] := cxRectBounds(Left-1, Top, 1, Bottom - Top + 1);
      end;
      if bTop in ABorders then
      begin
        SetLength(ARects, Length(ARects) + 1);
        ARects[High(ARects)] := cxRectBounds(Left-1, Top - 1, Right - Left + 1, 1);
      end;
      if bRight in ABorders then
      begin
        SetLength(ARects, Length(ARects) + 1);
        ARects[High(ARects)] := cxRectBounds(Right-1, Top, 1, Bottom - Top + 1);
      end;
      if bBottom in ABorders then
      begin
        SetLength(ARects, Length(ARects) + 1);
        ARects[High(ARects)] := cxRectBounds(Left-1, Bottom, Right - Left + 1, 1);
      end;
    end;
  end;
begin
  ARow := AHeaderViewInfo.Row;
  IsBottomNeeded := True;
  if (ARow is TcxCategoryRow) and (ARow.HasChildren)  then
    IsBottomNeeded := False;
  ABorders := [bLeft, bTop, bBottom];
  if not IsBottomNeeded then
    ABorders := ABorders - [bBottom, bTop];
  for I := AHeaderViewInfo.RowIndents.Count - 1 downto 0 do
  begin
    ARow := ARow.Parent;
    AddIndentRect(AHeaderViewInfo.RowIndents[I].Bounds, ABorders, Result);
    ABorders := ABorders - [bTop, bBottom];
  end;
  IsTopNeeded := False;
  if AHeaderViewInfo.Row is TcxCategoryRow then
    IsTopNeeded := True;
  ARow := AHeaderViewInfo.Row;
  if IsTopNeeded then
    ABorders := [bLeft, bTop, bBottom]
  else
    ABorders := [bLeft, bBottom];
  for I := AHeaderViewInfo.CategoryIndents.Count - 1 downto 0 do
  begin
    ARow := ARow.Parent;
    if I = AHeaderViewInfo.CategoryIndents.Count - 1 then
       ABorders := ABorders - [bBottom]
    else
    begin
      if ARow is TcxCategoryRow then
          ABorders := ABorders - [bBottom, bTop]
      else
        if IsTopNeeded then
          ABorders := ABorders - [bBottom]
        else
          ABorders := ABorders - [bBottom, bTop];
    end;
    AddIndentRect(AHeaderViewInfo.CategoryIndents[I].Bounds, ABorders, Result);
  end;
end;


end.
