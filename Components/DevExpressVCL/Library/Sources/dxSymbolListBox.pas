{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSymbolListBox;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, ComCtrls, cxClasses, cxListBox,
  Generics.Defaults, Generics.Collections;

type
  TdxCustomSymbolListBox = class(TdxCustomListBox)
  private
    FColumnCount: Integer;
    FLockCount: Integer;
    FItemsAreaRowCount: Integer;
    FUnicodeChars: TList<Word>;
    FSelectedIndexSymbol: integer;
    FSelectedRow: Integer;
    FSelectedColumn: Integer;
    FOnSelectedIndexSymbolChanged: TNotifyEvent;
    procedure CalculateRowCount;
    function GetFontName: string;
    function GetSelectedChar: Char;
    function GetUnicodeChars(Index: Integer): Char;
    function GetSelectedCharCode: Word;
    procedure SetColumnCount(const ARect: TRect);
    procedure SetSelectedChar(AChar: Char); virtual;
    procedure SetSelectedCharCode(const Value: Word);
    procedure SetSelectedIndexSymbol(const Value: integer);
    procedure UpdateSelectedPos;
  protected
    procedure DoSelectedIndexSymbolChanged;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DrawItemRect(const R: TRect; AItem: TdxCustomListBoxItem); virtual;
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    function GetLineThickness: Integer; virtual;
    function GetTextFlags: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Resize; override;
    function OrdToString(AOrd: Word): string;
    procedure SetFontName(const Value: string); virtual;
    property ColumnCount: Integer read FColumnCount;
    property UnicodeChars[Index: Integer]: Char read GetUnicodeChars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddChar(AChar: Char);
    procedure ClearChars;
    function IsCodeChar(ACode: Word): Boolean;
    property FontName: string read GetFontName write SetFontName;
    property SelectedChar: Char read GetSelectedChar write SetSelectedChar;
    property SelectedCharCode: Word read GetSelectedCharCode write SetSelectedCharCode;
    property SelectedIndexSymbol: integer read FSelectedIndexSymbol write SetSelectedIndexSymbol;
    property OnSelectedIndexSymbolChanged: TNotifyEvent read FOnSelectedIndexSymbolChanged write FOnSelectedIndexSymbolChanged;
  end;

  TdxSymbolListBox = class(TdxCustomSymbolListBox)
  private
    FIsSymbolFont: Boolean;
  protected
    procedure DrawBackground; override;
    function GetLineThickness: Integer; override;
    procedure SetCharsContainsInFont(const AFontName: string);
    procedure SetFontName(const Value: string); override;
    property IsSymbolFont: Boolean read FIsSymbolFont;
  published
    property FontName;
    property ItemHeight;
  end;

  TdxSimpleSymbolListBox = class(TdxCustomSymbolListBox)
  public const
    SelectRectangleMargin = 2;
    FocusRectangleMargin = 1;
    SelectRectangleThickness = 2;
  private
    FIsActive: Boolean;
  protected
    procedure DrawItemRect(const R: TRect; AItem: TdxCustomListBoxItem); override;
    procedure DrawSelectionRectangle(const R: TRect);
    procedure DrawItemBackground(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure Resize; override;
  public
    property IsActive: Boolean read FIsActive write FIsActive;
    property UnicodeChars;
  published
    property OnMouseDown;
    property FontName;
    property ItemHeight;
    property OnKeyDown;
  end;

implementation

uses
  Types, Math, Character, dxTypeHelpers, cxGeometry,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.UnitDocumentConverter;

{ TdxCustomCharsListBox }

constructor TdxCustomSymbolListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncrementalSearch := False;
  FUnicodeChars := TList<Word>.Create;
end;

destructor TdxCustomSymbolListBox.Destroy;
begin
  FreeAndNil(FUnicodeChars);
  inherited Destroy;
end;

procedure TdxCustomSymbolListBox.AddChar(AChar: Char);
var
  AValue: Word;
begin
  AValue := Ord(AChar);
  FUnicodeChars.Add(AValue);
end;

procedure TdxCustomSymbolListBox.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomSymbolListBox.CalculateRowCount;
var
  ARowCount: Integer;
  I: Integer;
begin
  SetColumnCount(ItemsAreaRect);
  if ColumnCount = 0 then
    Exit;
  ARowCount := (FUnicodeChars.Count + ColumnCount - 1) div ColumnCount;
  FItemsAreaRowCount := ItemsAreaRect.Height div ItemHeight;
  Items.BeginUpdate;
  try
    Items.Clear;
    for I := 0 to ARowCount - 1 do
      Items.Add('');
  finally
    Items.EndUpdate;
  end;
end;

procedure TdxCustomSymbolListBox.ClearChars;
begin
  FUnicodeChars.Clear;
end;

procedure TdxCustomSymbolListBox.SetColumnCount(const ARect: TRect);
begin
  FColumnCount := Max(ARect.Width div ItemHeight, 1);
end;

procedure TdxCustomSymbolListBox.SetFontName(const Value: string);
begin
  Font.Name := Value;
  Font.Size := MulDiv(MulDiv(ItemHeight - 2 * GetLineThickness, 2, 3), 72, Font.PixelsPerInch);
end;

procedure TdxCustomSymbolListBox.SetSelectedChar(AChar: Char);
var
  I: Integer;
begin
  for I := 0 to FUnicodeChars.Count - 1 do
    if OrdToString(FUnicodeChars[I])[1] = AChar then
    begin
      SelectedIndexSymbol := I;
      Exit;
    end;
  SelectedIndexSymbol := 0;
end;

procedure TdxCustomSymbolListBox.SetSelectedCharCode(const Value: Word);
var
  AIndex: Integer;
begin
  if FUnicodeChars.BinarySearch(Value, AIndex) then
    SelectedIndexSymbol := AIndex
  else
    SelectedIndexSymbol := 0;
end;

procedure TdxCustomSymbolListBox.SetSelectedIndexSymbol(const Value: integer);
begin
  if FSelectedIndexSymbol = Value then
    Exit;
  FSelectedIndexSymbol := Value;
  UpdateSelectedPos;
  ItemIndex := FSelectedIndexSymbol div ColumnCount;
  DoSelectedIndexSymbolChanged;
  Refresh;
end;

procedure TdxCustomSymbolListBox.UpdateSelectedPos;
begin
  FSelectedColumn := SelectedIndexSymbol mod ColumnCount;
  FSelectedRow := ItemIndex - TopIndex;
end;

function TdxCustomSymbolListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  UpdateSelectedPos;
end;

procedure TdxCustomSymbolListBox.DoSelectedIndexSymbolChanged;
begin
  if Assigned(FOnSelectedIndexSymbolChanged) then
    FOnSelectedIndexSymbolChanged(Self);
end;

procedure TdxCustomSymbolListBox.DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);

  procedure DrawSymbols;
  var
    ASelRect: TRect;
    ATextRect: TRect;
    AIndex: Integer;
    I: Integer;

    function GetBoundsSelectRectangle: TRect;
    begin
      Result.Top := ATextRect.Top;
      Result.Bottom := ATextRect.Bottom - GetLineThickness;
      Result.Left := ATextRect.Left + (SelectedIndexSymbol mod ColumnCount) * ItemHeight;
      Result.Right := Result.Left + ItemHeight - GetLineThickness;
    end;

  begin
    AIndex := AItem.Index * ColumnCount;
    ATextRect := R;
    ATextRect.Width := ItemHeight;
    if SelectedIndexSymbol div ColumnCount = AItem.Index then
    begin
      ASelRect := GetBoundsSelectRectangle;
      DrawItemRect(ASelRect, AItem);
    end;

    Canvas.Font := Font;
    for I := 0 to ColumnCount - 1 do
      if AIndex < FUnicodeChars.Count then
      begin
        if I = SelectedIndexSymbol mod ColumnCount then
          Canvas.Font.Color := GetTextColor(AItem, AState)
        else
          Canvas.Font.Color := GetTextColor(AItem, cxbsNormal);
        Canvas.DrawTexT(OrdToString(FUnicodeChars[AIndex]), ATextRect, GetTextFlags, True, ra0);
        ATextRect.Offset(ItemHeight, 0);
        Inc(AIndex);
      end;
  end;

begin
  SetColumnCount(R);
  DrawSymbols;
end;

procedure TdxCustomSymbolListBox.DrawItemRect(const R: TRect; AItem: TdxCustomListBoxItem);
begin
  DrawItemBackground(R, AItem, cxbsPressed);
  if Focused then
    Canvas.DrawFocusRect(R);
end;

procedure TdxCustomSymbolListBox.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    CalculateRowCount;
    Refresh;
    FLockCount := 0;
  end;
end;

procedure TdxCustomSymbolListBox.FocusEnter;
begin
  Refresh;
end;

procedure TdxCustomSymbolListBox.FocusLeave;
begin
  Refresh;
end;

function TdxCustomSymbolListBox.GetFontName: string;
begin
  Result := Font.Name;
end;

function TdxCustomSymbolListBox.GetLineThickness: Integer;
begin
  Result := 0;
end;

function TdxCustomSymbolListBox.GetSelectedChar: Char;
begin
  if (SelectedIndexSymbol >=0) and (SelectedIndexSymbol < FUnicodeChars.Count) then
    Result := OrdToString(FUnicodeChars[SelectedIndexSymbol])[1]
  else
    Result := #0;
end;

function TdxCustomSymbolListBox.GetSelectedCharCode: Word;
begin
  if (SelectedIndexSymbol < 0) or (SelectedIndexSymbol >= FUnicodeChars.Count) then
    Result := 0
  else
    Result := FUnicodeChars[SelectedIndexSymbol];
end;

function TdxCustomSymbolListBox.GetTextFlags: Integer;
begin
  Result := DT_SINGLELINE or DT_CENTER or DT_VCENTER;
end;

function TdxCustomSymbolListBox.GetUnicodeChars(Index: Integer): Char;
begin
  Result := OrdToString(FUnicodeChars[Index])[1];
end;

function TdxCustomSymbolListBox.IsCodeChar(ACode: Word): Boolean;
begin
  Result := FUnicodeChars.Contains(ACode);
end;

procedure TdxCustomSymbolListBox.KeyDown(var Key: Word; Shift: TShiftState);

  procedure KeyDownPageUp;
  begin
    if TopIndex = 0 then
      Exit;
    if (TopIndex - FItemsAreaRowCount) >= 0 then
    begin
      SelectedIndexSymbol := FSelectedIndexSymbol - FItemsAreaRowCount * ColumnCount;
      TopIndex := TopIndex - FItemsAreaRowCount;
    end
    else
    begin
      SelectedIndexSymbol := FSelectedIndexSymbol - TopIndex * ColumnCount;
      TopIndex := 0;
    end;
    ItemIndex := TopIndex + FSelectedRow;
  end;

  procedure KeyDownPageDown;
  var
    ANewIndex: Integer;
    ADeltaColumn: Integer;
    ADeltaScroll: Integer;
    ABottomBorder: Integer;
  begin
    if Items.Count < FItemsAreaRowCount then
      Exit;
    ANewIndex := SelectedIndexSymbol + FItemsAreaRowCount * ColumnCount;
    ADeltaScroll := FItemsAreaRowCount;
    ABottomBorder := TopIndex + 2 * FItemsAreaRowCount;
    if ABottomBorder > Items.Count then
    begin
      ADeltaColumn := ABottomBorder - Items.Count;
      Dec(ANewIndex, ADeltaColumn * ColumnCount);
      Dec(ADeltaScroll, ADeltaColumn);
    end;
    if (ANewIndex < FUnicodeChars.Count) then
    begin
      TopIndex := TopIndex + ADeltaScroll;
      ItemIndex := ItemIndex + ADeltaScroll;
      SelectedIndexSymbol := ANewIndex;
      Refresh;
    end;
  end;

  procedure KeyDownEnd;
  begin
    SelectedIndexSymbol := FUnicodeChars.Count - 1;
    FSelectedRow := FItemsAreaRowCount - 1;
    FSelectedColumn := FSelectedIndexSymbol mod ColumnCount;
    ItemIndex := Items.Count - 1;
  end;

  procedure KeyDownHome;
  begin
    SelectedIndexSymbol := 0;
  end;

  procedure KeyDownLeft;
  begin
    if SelectedIndexSymbol = 0 then
      Exit;
    if FSelectedColumn > 0 then
      Dec(FSelectedColumn)
    else
    begin
      if FSelectedRow = 0 then
        TopIndex := TopIndex - 1
      else
        Dec(FSelectedRow);
      ItemIndex := ItemIndex - 1;
      FSelectedColumn := ColumnCount - 1;
    end;
    SelectedIndexSymbol := FSelectedIndexSymbol - 1;
    Refresh;
  end;

  procedure KeyDownUp;
  var
    ANewIndex: Integer;
  begin
    ANewIndex := SelectedIndexSymbol - ColumnCount;
    if ANewIndex < 0 then
      Exit;
    if FSelectedRow = 0 then
      TopIndex := TopIndex - 1
    else
      Dec(FSelectedRow);
    ItemIndex := ItemIndex - 1;
    SelectedIndexSymbol := ANewIndex;
    Refresh;
  end;

  procedure KeyDownRight;
  begin
    if SelectedIndexSymbol = FUnicodeChars.Count - 1 then
      Exit;
    if FSelectedColumn < (ColumnCount - 1) then
      Inc(FSelectedColumn)
    else
    begin
      if FSelectedRow >= (FItemsAreaRowCount - 1) then
        TopIndex := TopIndex + 1
      else
        inc(FSelectedRow);
      ItemIndex := ItemIndex + 1;
      FSelectedColumn := 0;
    end;
    SelectedIndexSymbol := FSelectedIndexSymbol + 1;
    Refresh;
  end;

  procedure KeyDownDown;
  var
    ANewIndex: Integer;
  begin
    ANewIndex := SelectedIndexSymbol + ColumnCount;
    if ANewIndex >= FUnicodeChars.Count then
      Exit;
    if FSelectedRow >= (FItemsAreaRowCount - 1) then
      TopIndex := TopIndex + 1
    else
      Inc(FSelectedRow);
    ItemIndex := ItemIndex + 1;
    SelectedIndexSymbol := ANewIndex;
    Refresh;
  end;

begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
  case Key of
    VK_PRIOR:
      KeyDownPageUp;
    VK_NEXT:
      KeyDownPageDown;
    VK_END:
      KeyDownEnd;
    VK_HOME:
      KeyDownHome;
    VK_LEFT:
      KeyDownLeft;
    VK_UP:
      KeyDownUp;
    VK_RIGHT:
      KeyDownRight;
    VK_DOWN:
      KeyDownDown;
  else
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TdxCustomSymbolListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  SetSelectedChar(Key);
end;

procedure TdxCustomSymbolListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure UpdateSelectIndexAfterMouseDown;
  var
    ACharIndex: Integer;
    AItemIndex: Integer;
    AColumnIndex: Integer;
  begin
    AItemIndex := ItemAtPos(Point(X, Y));
    AColumnIndex := X div ItemHeight;
    ACharIndex := AItemIndex * ColumnCount + AColumnIndex;
    if ACharIndex < FUnicodeChars.Count then
    begin
      SelectedIndexSymbol := ACharIndex;
      Refresh;
    end;
  end;

begin
  inherited MouseDown(Button, Shift, X, Y);
  UpdateSelectIndexAfterMouseDown;
end;

function TdxCustomSymbolListBox.OrdToString(AOrd: Word): string;
begin
  if AOrd > 255 then
    Result := Char(AOrd)
  else
    Result := UnicodeString(AnsiChar(AOrd));
end;

procedure TdxCustomSymbolListBox.Resize;
begin
  inherited;
  CalculateRowCount;
  UpdateSelectedPos;
end;

{ TdxSymbolListBox }

procedure TdxSymbolListBox.DrawBackground;

  procedure DrawGrid(R: TRect);
  var
    AStartPos: Integer;
    ARight: Integer;
    ABottom: Integer;
  begin
    ARight := ColumnCount * ItemHeight + BorderSize;
    ABottom := FItemsAreaRowCount * ItemHeight;
    AStartPos := R.Left + ItemHeight;
    while AStartPos <= ARight do
    begin
      Canvas.Line(Point(AStartPos, R.Top), Point(AStartPos, ABottom));
      Inc(AStartPos, ItemHeight);
    end;
    AStartPos := R.Top + ItemHeight;
    while AStartPos <= ABottom do
    begin
      Canvas.Line(Point(R.Left, AStartPos), Point(ARight, AStartPos));
      Inc(AStartPos, ItemHeight);
    end;
  end;

begin
  inherited DrawBackground;
  DrawGrid(ClientRect);
end;

function TdxSymbolListBox.GetLineThickness: Integer;
begin
  Result := 1;
end;

procedure TdxSymbolListBox.SetCharsContainsInFont(const AFontName: string);
var
  I: Integer;
  AChar: Char;
  AIsCharCategoryControl: Boolean;
  AIsCharCategoryPrivateUse: Boolean;
  AFontCharacterSet: TdxFontCharacterSet;
  AFontCashe: TdxFontCache;
begin
  FUnicodeChars.Clear;
  AFontCashe := TdxFontCacheManager.GetFontCache(TdxDocumentLayoutUnit.Document,
    TdxDocumentLayoutUnitDocumentConverter.DefaultDpi);
  AFontCharacterSet := AFontCashe.GetFontCharacterSet(AFontName);
  if IsSymbolFont then
  begin
    for I := 32 to 255 do
      FUnicodeChars.Add(I)
  end
  else
  begin
    for I := 0 to MAXWORD - 1 do
    begin
      AChar := OrdToString(I)[1];
      AIsCharCategoryControl := {$IFDEF DELPHIXE4}AChar.IsControl{$ELSE}TCharacter.IsControl(AChar){$ENDIF};
      AIsCharCategoryPrivateUse := {$IFDEF DELPHIXE4}AChar.GetUnicodeCategory{$ELSE}TCharacter.GetUnicodeCategory(AChar){$ENDIF} = TUnicodeCategory.ucPrivateUse;
      if AFontCharacterSet.ContainsChar(AChar) and not AIsCharCategoryControl and not AIsCharCategoryPrivateUse then
        FUnicodeChars.Add(I);
    end;
  end;
  CalculateRowCount;
end;

procedure TdxSymbolListBox.SetFontName(const Value: string);
var
  AFontInfo: TdxTrueTypeFontInfo;
  AOldCodeChar: Word;
begin
  inherited SetFontName(Value);
  if TdxGdiFontCache.SystemTrueTypeFonts.TryGetValue({$IFDEF DELPHIXE4}Value.ToUpper{$ELSE}ToUpper(Value){$ENDIF}, AFontInfo) then
  begin
    FIsSymbolFont := (AFontInfo.FontType and TdxTrueTypeFontInfo.SymbolFontType) <> 0;
    SetCharsContainsInFont(Value);
    AOldCodeChar := SelectedCharCode;
    if not IsCodeChar(AOldCodeChar) then
      SelectedCharCode := FUnicodeChars.First;
  end;
end;

{ TdxSimpleSymbolListBox }

procedure TdxSimpleSymbolListBox.DrawItemBackground(const R: TRect; AItem: TdxCustomListBoxItem;
  AState: TcxButtonState);
begin
  if AState = cxbsPressed then
    Canvas.FillRect(R, GetBackgroundColor);
end;

procedure TdxSimpleSymbolListBox.DrawItemRect(const R: TRect; AItem: TdxCustomListBoxItem);
begin
  inherited DrawItemRect(R, AItem);
  if IsActive then
    DrawSelectionRectangle(R);
end;

procedure TdxSimpleSymbolListBox.DrawSelectionRectangle(const R: TRect);
var
  ARect: TRect;
  ASelectRectangleMargin: Integer;
begin
  ASelectRectangleMargin := SelectRectangleMargin;
  ARect := cxRectInflate(R, -ASelectRectangleMargin, -ASelectRectangleMargin);
  Canvas.FrameRect(ARect, LookAndFeelPainter.DefaultSelectionColor, SelectRectangleThickness);
end;

procedure TdxSimpleSymbolListBox.Resize;
begin
  inherited Resize;
  ItemHeight := Max(1, ClientBounds.Height);
end;

end.
