{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetInplaceEdit;

{$I cxVer.Inc}
{$R dxSpreadSheetInplaceEdit.res}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, Controls, dxCore, cxControls, cxGraphics, cxRichEdit, cxLookAndFeels,
  dxAutoCompleteWindow, cxImageList, cxListBox, cxLookAndFeelPainters, dxCoreGraphics, dxCustomHint, cxHint,
  cxDrawTextUtils;

type

  { IdxSpreadSheetInplaceEditController }

  IdxSpreadSheetInplaceEditController = interface
  ['{9EA3BEBF-A536-4983-9D36-A00919D6E432}']
    function IsAutoCompleteAllowed: Boolean;
    function IsAutoCompleteSuggestionsHintsAllowed: Boolean;
    procedure KeyDown(var Key: Word; ShiftState: TShiftState);
    procedure Post(IsArrayFormula: Boolean = False);
  end;

  { TdxSpreadSheetCustomInplaceEdit }

  TdxSpreadSheetCustomInplaceEdit = class abstract(TcxRichEdit)
  strict private
    FAutoCompleteWindow: TdxCustomAutoCompleteWindow;

    procedure HandlerAutoCompleteWindowSelectItem(Sender: TObject);
  protected
    procedure ChangeHandler(Sender: TObject); override;
    procedure DoExit; override;
    procedure DoSelectionChanged; override;

    function GetController: IdxSpreadSheetInplaceEditController; virtual; abstract;
    function GetInnerEditClass: TControlClass; override;
    function IsFormattingSupported: Boolean; virtual;
    function IsFormula: Boolean; virtual;
    procedure UpdateKeys; virtual;

    // Keyboard
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function ProcessReturnKey(Shift: TShiftState): Boolean; virtual;
    function ProcessShortCutChar(const Key: Char): Boolean; virtual;
    function ProcessTabKey(Shift: TShiftState): Boolean; virtual;

    // auto-complete window
    function CreateAutoCompleteWindow: TdxCustomAutoCompleteWindow; virtual;
    procedure HideAutoCompleteWindow; virtual;
    function GetAutoCompleteWindowOwnerBounds: TRect;
    function IsAutoCompleteWindowVisible: Boolean;
    procedure PopulateAutoCompleteSuggestions(const ASearchText: string; ASuggestions: TStrings); virtual;
    procedure RefreshAutoCompleteSuggestions(const ASearchText: string); overload;
    procedure RefreshAutoCompleteSuggestions; overload;
    procedure RefreshAutoCompleteWindow(const ASearchText: string; ASuggestions: TStrings); virtual;
    //
    property AutoCompleteWindow: TdxCustomAutoCompleteWindow read FAutoCompleteWindow;
  public
    procedure BeforeDestruction; override;
    function HasPopupWindow: Boolean; override;
    function IsInImeComposition: Boolean;
    //
    property Controller: IdxSpreadSheetInplaceEditController read GetController;
  end;

  { TdxSpreadSheetCustomInplaceInnerEdit }

  TdxSpreadSheetCustomInplaceInnerEdit = class(TcxRichInnerEdit)
  strict private
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
  end;

  { TdxSpreadSheetInplaceEditAutoCompleteInnerListBox }

  TdxSpreadSheetInplaceEditAutoCompleteInnerListBox = class(TdxCustomAutoCompleteInnerListBox)
  strict private
    FColorPalette: IdxColorPalette;

    function GetColorPalette: IdxColorPalette;
  protected
    procedure DrawItemImage(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    function IsSizeGripVisible: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

  { TdxSpreadSheetInplaceEditAutoCompleteWindow }

  TdxSpreadSheetInplaceEditAutoCompleteWindow = class(TdxCustomAutoCompleteWindow)
  strict private
    FHintWindow: THintWindow;

    procedure HandlerItemIndexChanged(Sender: TObject);
  protected
    function CalculateSize: TSize; override;
    function CreateHintWindow: THintWindow; virtual;
    function CreateInnerListBox: TdxCustomAutoCompleteInnerListBox; override;
    procedure DoHide; override;
    function GetObjectHint(AObject: TObject): string; virtual;
    function GetObjectImageIndex(AObject: TObject): TcxImageIndex; virtual;
    procedure UpdateHintWindow;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;
    procedure Populate(AList: TStrings); override;
    procedure Popup(AFocusedControl: TWinControl); override;
  end;

  { TdxSpreadSheetInplaceEditAutoCompleteHintWindow }

  TdxSpreadSheetInplaceEditAutoCompleteHintWindow = class(THintWindow)
  strict private const
    DrawTextFlags = CXTO_WORDBREAK or CXTO_NOPREFIX;
  strict private
    function GetOwnerEx: TdxSpreadSheetInplaceEditAutoCompleteWindow;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure NCPaint(DC: HDC); override;
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    //
    property Owner: TdxSpreadSheetInplaceEditAutoCompleteWindow read GetOwnerEx;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

  { TdxSpreadSheetInplaceEditAutoCompleteWindowImages }

  TdxSpreadSheetInplaceEditAutoCompleteWindowImages = class
  strict private
    class var FImages: TcxCustomImageList;

    class procedure ImportImage(const AResName: string);
  public
    class procedure Finalize;
    class function GetImageIndex(AObject: TObject): Integer;
    class function GetImageList: TcxCustomImageList;
  end;

implementation

uses
  StrUtils, SysUtils, Math, Graphics, cxGeometry, dxGDIPlusClasses, cxRichEditUtils,
  dxSpreadSheetCore,
  dxSpreadSheetCoreHelpers,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetFormulasHelpers,
  dxSpreadSheetFunctions,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type
  TcxRichInnerEditAccess = class(TcxRichInnerEdit);

function GetActualMasterLookAndFeel(AInplaceEdit: TdxSpreadSheetCustomInplaceEdit): TcxLookAndFeel;
var
  AIntf: IdxSpreadSheet;
begin
  if Supports(AInplaceEdit.Owner, IdxSpreadSheet, AIntf) then
    Result := AIntf.GetControl.LookAndFeel
  else
    Result := AInplaceEdit.Style.LookAndFeel;
end;

{ TdxSpreadSheetCustomInplaceEdit }

procedure TdxSpreadSheetCustomInplaceEdit.BeforeDestruction;
begin
  inherited;
  HideAutoCompleteWindow;
end;

function TdxSpreadSheetCustomInplaceEdit.HasPopupWindow: Boolean;
begin
  Result := inherited or IsAutoCompleteWindowVisible;
end;

function TdxSpreadSheetCustomInplaceEdit.IsInImeComposition: Boolean;
begin
  Result := HasInnerEdit and TcxRichInnerEditAccess(InnerRich).FInImeComposition;
end;

procedure TdxSpreadSheetCustomInplaceEdit.ChangeHandler(Sender: TObject);
begin
  inherited;
  if not AreChangeEventsLocked then
    RefreshAutoCompleteSuggestions;
end;

procedure TdxSpreadSheetCustomInplaceEdit.DoExit;
begin
  inherited;
  HideAutoCompleteWindow;
end;

procedure TdxSpreadSheetCustomInplaceEdit.DoSelectionChanged;
begin
  HideAutoCompleteWindow;
  inherited;
end;

function TdxSpreadSheetCustomInplaceEdit.GetInnerEditClass: TControlClass;
begin
  Result := TdxSpreadSheetCustomInplaceInnerEdit;
end;

function TdxSpreadSheetCustomInplaceEdit.IsFormattingSupported: Boolean;
begin
  Result := not IsFormula and dxSpreadSheetTextService.IsRTFSupported and not Properties.PlainText;
end;

function TdxSpreadSheetCustomInplaceEdit.IsFormula: Boolean;
begin
  Result := dxSpreadSheetIsFormula(Text);
end;

procedure TdxSpreadSheetCustomInplaceEdit.UpdateKeys;
begin
  if IsAutoCompleteWindowVisible then
    Keys := Keys + [kTab]
  else
    Keys := Keys - [kTab];

  InnerRich.WantTabs := kTab in Keys;
end;

procedure TdxSpreadSheetCustomInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
const
  ShortcutMap: array[0..2] of Char = (^B, ^I, ^U);
begin
  case Key of
    VK_UP, VK_DOWN:
      if AutoCompleteWindow <> nil then
      begin
        AutoCompleteWindow.ProcessNavigationKey(Key, Shift);
        Key := 0;
      end;

    VK_TAB:
      if ProcessTabKey(Shift) then
        Key := 0;

    VK_RETURN:
      if ProcessReturnKey(Shift) then
        Key := 0;

    VK_ESCAPE:
      if AutoCompleteWindow <> nil then
      begin
        HideAutoCompleteWindow;
        Key := 0;
      end;
  end;

  if ssCtrl in Shift then
  begin
    if dxCharInSet(Char(Key), ['2', '3', '4']) then
      ProcessShortCutChar(ShortCutMap[Key - Word('2')]);
    if dxCharInSet(Char(Key), ['1'..'5', 'E', 'J', 'L', 'R']) or (ssShift in Shift) and (Char(Key) = 'A') then
      Key := 0;
  end;

  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TdxSpreadSheetCustomInplaceEdit.KeyPress(var Key: Char);
begin
  if ProcessShortCutChar(Key) then
    Key := #0;
  if Key = #9 then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TdxSpreadSheetCustomInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_TAB then
    Key := 0;
  inherited;
end;

function TdxSpreadSheetCustomInplaceEdit.ProcessReturnKey(Shift: TShiftState): Boolean;
const
  ArrayFormulaShiftStates: TShiftState = [ssCtrl, ssShift];
  ValidShiftStatesForEditing: TShiftState = [ssShift, ssCtrl, ssAlt];
var
  ABalance: Integer;
  AKey: Word;
  AShiftStates: TShiftState;
begin
  Result := Controller <> nil;
  if Result then
  begin
    AShiftStates := ValidShiftStatesForEditing * Shift;
    if (AShiftStates = [ssShift]) or (AShiftStates  = []) then
    begin
      if IsFormula then
      begin
        ABalance := TdxModifyFormulasHelper.GetBracketsBalance(Text);
        if ABalance > 0 then
          Text := Text + DupeString(')', ABalance);
      end;

      AKey := VK_RETURN;
      Controller.Post;
      Controller.KeyDown(AKey, Shift);
    end
    else
    begin
      if (AShiftStates = [ssCtrl]) or (AShiftStates = ArrayFormulaShiftStates) then
        Controller.Post(AShiftStates = ArrayFormulaShiftStates);
      if AShiftStates = [ssAlt] then
        SelText := dxCRLF;
    end;
  end;
end;

function TdxSpreadSheetCustomInplaceEdit.ProcessShortCutChar(const Key: Char): Boolean;
const
  HotKeys: array[TFontStyle] of Char = (^B, ^I, ^U, ^S);
var
  AStyle: TFontStyle;
begin
  Result := False;
  if IsFormattingSupported then
  begin
    for AStyle := Low(TFontStyle) to High(TFontStyle) do
      if HotKeys[AStyle] = Key then
      begin
        if AStyle in SelAttributes.Style then
          SelAttributes.Style := SelAttributes.Style - [AStyle]
        else
          SelAttributes.Style := SelAttributes.Style + [AStyle];

        Result := True;
      end;
  end;
end;

function TdxSpreadSheetCustomInplaceEdit.ProcessTabKey(Shift: TShiftState): Boolean;
begin
  Result := (AutoCompleteWindow <> nil) and AutoCompleteWindow.HasSelectedItem;
  if Result then
  begin
    HandlerAutoCompleteWindowSelectItem(AutoCompleteWindow);
    HideAutoCompleteWindow;
  end;
end;

function TdxSpreadSheetCustomInplaceEdit.CreateAutoCompleteWindow: TdxCustomAutoCompleteWindow;
begin
  Result := TdxSpreadSheetInplaceEditAutoCompleteWindow.Create(Self);
end;

procedure TdxSpreadSheetCustomInplaceEdit.HideAutoCompleteWindow;
begin
  FreeAndNil(FAutoCompleteWindow);
  UpdateKeys;
end;

function TdxSpreadSheetCustomInplaceEdit.GetAutoCompleteWindowOwnerBounds: TRect;
begin
  Result := ClientBounds;
  Inc(Result.Left, cxRichEditGetCharPosition(InnerRich.Handle, TcxRichInnerEditAccess(InnerRich).RichVersion, SelStart).X);
end;

function TdxSpreadSheetCustomInplaceEdit.IsAutoCompleteWindowVisible: Boolean;
begin
  Result := AutoCompleteWindow <> nil;
end;

procedure TdxSpreadSheetCustomInplaceEdit.RefreshAutoCompleteSuggestions;
var
  ASelStart: Integer;
  AText: string;
begin
  if IsFormula and Focused then
  begin
    AText := Text;
    ASelStart := SelStart;
    while (ASelStart > 0) and not dxIsOperandSeparator(AText[ASelStart]) do
      Dec(ASelStart);
    RefreshAutoCompleteSuggestions(Copy(AText, ASelStart + 1, SelStart - ASelStart));
  end
  else
    HideAutoCompleteWindow;
end;

procedure TdxSpreadSheetCustomInplaceEdit.PopulateAutoCompleteSuggestions(const ASearchText: string; ASuggestions: TStrings);

  function GetActiveSheet: TdxSpreadSheetCustomView;
  var
    AIntf: IdxSpreadSheet;
  begin
    if Supports(Owner, IdxSpreadSheet, AIntf) then
      Result := AIntf.GetControl.ActiveSheet
    else
      Result := nil;
  end;

var
  AInfo: TdxSpreadSheetFunctionInfo;
  ARepository: TdxSpreadSheetFunctionsRepository;
  I: Integer;
begin
  TdxSpreadSheetDefinedNameHelper.Enum(GetActiveSheet,
    procedure (ADefinedName: TdxSpreadSheetDefinedName)
    begin
      if StartsText(ASearchText, ADefinedName.Caption) then
        ASuggestions.AddObject(ADefinedName.Caption, ADefinedName);
    end);

  ARepository := dxSpreadSheetFunctionsRepository;
  for I := 0 to ARepository.Count - 1 do
  begin
    AInfo := ARepository.Items[I];
    if Assigned(AInfo.Proc) then
    begin
      if StartsText(ASearchText, AInfo.Name) then
        ASuggestions.AddObject(AInfo.Name, AInfo);
    end;
  end;
end;

procedure TdxSpreadSheetCustomInplaceEdit.RefreshAutoCompleteSuggestions(const ASearchText: string);
var
  ASuggestions: TStringList;
begin
  if ASearchText <> '' then
  begin
    ASuggestions := TStringList.Create;
    try
      PopulateAutoCompleteSuggestions(ASearchText, ASuggestions);
      ASuggestions.Sort;
      RefreshAutoCompleteWindow(ASearchText, ASuggestions);
    finally
      ASuggestions.Free;
    end;
  end
  else
    HideAutoCompleteWindow;
end;

procedure TdxSpreadSheetCustomInplaceEdit.RefreshAutoCompleteWindow(const ASearchText: string; ASuggestions: TStrings);
begin
  if (ASuggestions.Count > 0) and Controller.IsAutoCompleteAllowed then
  begin
    if AutoCompleteWindow = nil then
    begin
      FAutoCompleteWindow := CreateAutoCompleteWindow;
      FAutoCompleteWindow.OnSelectItem := HandlerAutoCompleteWindowSelectItem;
      FAutoCompleteWindow.ShowHint := Controller.IsAutoCompleteSuggestionsHintsAllowed;
      UpdateKeys;
    end;
    AutoCompleteWindow.Populate(ASuggestions);
    AutoCompleteWindow.SearchText := ASearchText;
    AutoCompleteWindow.SelectedText := ASuggestions[0];
    AutoCompleteWindow.OwnerBounds := GetAutoCompleteWindowOwnerBounds;
    AutoCompleteWindow.Adjustable := True;
    AutoCompleteWindow.Popup(Self);
  end
  else
    HideAutoCompleteWindow;
end;

procedure TdxSpreadSheetCustomInplaceEdit.HandlerAutoCompleteWindowSelectItem(Sender: TObject);
var
  ASearchTextLength: Integer;
  AText: string;
begin
  if AutoCompleteWindow.HasSelectedItem then
  begin
    AText := AutoCompleteWindow.SelectedText;
    if AutoCompleteWindow.SelectedObject is TdxSpreadSheetFunctionInfo then
      AText := AText + '(';

    ASearchTextLength := Length(AutoCompleteWindow.SearchText);
    if ASearchTextLength > 0 then
    begin
      SelStart := SelStart - ASearchTextLength;
      SelLength := ASearchTextLength;
    end;
    SelText := AText;
  end;
  HideAutoCompleteWindow;
end;

{ TdxSpreadSheetInplaceEditAutoCompleteWindow }

constructor TdxSpreadSheetInplaceEditAutoCompleteWindow.Create(AOwnerControl: TWinControl);
var
  AInnerEdit: TdxSpreadSheetCustomInplaceEdit;
begin
  inherited;
  AInnerEdit := AOwnerControl as TdxSpreadSheetCustomInplaceEdit;
  ItemsFont := AInnerEdit.ActiveStyle.GetVisibleFont;
  LookAndFeel.MasterLookAndFeel := GetActualMasterLookAndFeel(AInnerEdit);
  InnerListBox.OnClick := HandlerItemIndexChanged;
  HighlightSearchText := False;
  DisplayRowsCount := 12;
end;

destructor TdxSpreadSheetInplaceEditAutoCompleteWindow.Destroy;
begin
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteWindow.Populate(AList: TStrings);
var
  I: Integer;
begin
  inherited;
  InnerListBox.Items.BeginUpdate;
  try
    for I := 0 to InnerListBox.Count - 1 do
      InnerListBox.Items[I].ImageIndex := GetObjectImageIndex(InnerListBox.Items[I].Data);
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteWindow.Popup(AFocusedControl: TWinControl);
begin
  inherited;
  UpdateHintWindow;
end;

function TdxSpreadSheetInplaceEditAutoCompleteWindow.CalculateSize: TSize;
begin
  if Adjustable then
  begin
    InnerListBox.LayoutChanged;
    Result := InnerListBox.CalculateContentSize(DisplayRowsCount);
    Inc(Result.cx, 2 * dxAutoCompleteWindowBorderSize + NCWidth);
    Inc(Result.cy, 2 * dxAutoCompleteWindowBorderSize);
  end
  else
    Result := cxNullSize;
end;

function TdxSpreadSheetInplaceEditAutoCompleteWindow.CreateHintWindow: THintWindow;
begin
  Result := TdxSpreadSheetInplaceEditAutoCompleteHintWindow.Create(Self);
end;

function TdxSpreadSheetInplaceEditAutoCompleteWindow.CreateInnerListBox: TdxCustomAutoCompleteInnerListBox;
begin
  Result := TdxSpreadSheetInplaceEditAutoCompleteInnerListBox.Create(Self);
  Result.Images := TdxSpreadSheetInplaceEditAutoCompleteWindowImages.GetImageList;
  Result.CircularKeyboardNavigation := False;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteWindow.DoHide;
begin
  FreeAndNil(FHintWindow);
  inherited;
end;

function TdxSpreadSheetInplaceEditAutoCompleteWindow.GetObjectHint(AObject: TObject): string;
begin
  if AObject is TdxSpreadSheetFunctionInfo then
    Result := cxGetResourceString(TdxSpreadSheetFunctionInfo(AObject).DescriptionPtr)
  else
    Result := '';
end;

function TdxSpreadSheetInplaceEditAutoCompleteWindow.GetObjectImageIndex(AObject: TObject): TcxImageIndex;
begin
  Result := TdxSpreadSheetInplaceEditAutoCompleteWindowImages.GetImageIndex(AObject);
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteWindow.UpdateHintWindow;
var
  AHint: string;
  AHintSize: TSize;
  AMonitorRect: TRect;
  ARect: TRect;
  ARestSpace: Integer;
begin
  FreeAndNil(FHintWindow);

  AHint := GetObjectHint(InnerListBox.ItemObject);
  if (AHint <> '') and ShowHint then
  begin
    FHintWindow := CreateHintWindow;

    ARect := cxRectInflate(BoundsRect, ScaleFactor.Apply(3), 0);
    ARect.Top := InnerListBox.ClientToScreen(InnerListBox.ItemRect(InnerListBox.ItemIndex).TopLeft).Y;
    AHintSize := cxSize(FHintWindow.CalcHintRect(MaxWord, AHint, nil));
    AMonitorRect := Monitor.BoundsRect;
    ARestSpace := AMonitorRect.Right - ARect.Right;

    if (AHintSize.cx > ARestSpace) and (ARestSpace < ARect.Left - AMonitorRect.Left) then
    begin
      AHintSize := cxSize(FHintWindow.CalcHintRect(ARect.Left - AMonitorRect.Left, AHint, nil));
      ARect := cxRectSetRight(ARect, ARect.Left, AHintSize.cx);
      ARect := cxRectSetHeight(ARect, AHintSize.cy);
    end
    else
    begin
      AHintSize := cxSize(FHintWindow.CalcHintRect(ARestSpace, AHint, nil));
      ARect.Left := ARect.Right;
      ARect := cxRectSetSize(ARect, AHintSize);
    end;

    FHintWindow.ActivateHint(ARect, AHint);
  end;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteWindow.HandlerItemIndexChanged(Sender: TObject);
begin
  UpdateHintWindow;
end;

{ TdxSpreadSheetCustomInplaceInnerEdit }

procedure TdxSpreadSheetCustomInplaceInnerEdit.CMChildKey(var Message: TCMChildKey);
var
  AKey: Word;
begin
  if Message.CharCode = VK_ESCAPE then
  begin
    AKey := VK_ESCAPE;
    KeyDown(AKey, []);
    if AKey = 0 then
    begin
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

{ TdxSpreadSheetInplaceEditAutoCompleteInnerListBox }

procedure TdxSpreadSheetInplaceEditAutoCompleteInnerListBox.DrawItemImage(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AImageRect: TRect;
begin
  if IsImageAssigned(Images, AItem.ImageIndex) then
  begin
    AImageRect := cxRectCenter(R, ImageSize);
    cxDrawImage(Canvas.Handle, AImageRect, AImageRect, nil,
      Images, AItem.ImageIndex, idmNormal, False, 0, clNone, True, GetColorPalette);
  end;
end;

function TdxSpreadSheetInplaceEditAutoCompleteInnerListBox.IsSizeGripVisible: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteInnerListBox.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  FColorPalette := nil;
  inherited;
end;

function TdxSpreadSheetInplaceEditAutoCompleteInnerListBox.GetColorPalette: IdxColorPalette;
begin
  if FColorPalette = nil then
  begin
    FColorPalette := TdxSimpleColorPalette.Create(
      dxMakeAlphaColor(LookAndFeelPainter.DefaultControlTextColor), TdxAlphaColors.Empty);
  end;
  Result := FColorPalette;
end;

{ TdxSpreadSheetInplaceEditAutoCompleteWindowImages }

class procedure TdxSpreadSheetInplaceEditAutoCompleteWindowImages.Finalize;
begin
  FreeAndNil(FImages);
end;

class function TdxSpreadSheetInplaceEditAutoCompleteWindowImages.GetImageIndex(AObject: TObject): Integer;
begin
  Result := Ord(AObject is TdxSpreadSheetDefinedName);
end;

class function TdxSpreadSheetInplaceEditAutoCompleteWindowImages.GetImageList: TcxCustomImageList;
begin
  if FImages = nil then
  begin
    FImages := TcxImageList.CreateSize(16, 16);
    ImportImage('DXSPREADSHEET_AUTOCOMPLETE_FUNCTION');
    ImportImage('DXSPREADSHEET_AUTOCOMPLETE_DEFINEDNAME');
  end;
  Result := FImages;
end;

class procedure TdxSpreadSheetInplaceEditAutoCompleteWindowImages.ImportImage(const AResName: string);
var
  ASmartImage: TdxSmartImage;
begin
  ASmartImage := TdxSmartImage.Create;
  try
    ASmartImage.LoadFromResource(HInstance, AResName, 'SVG');
    FImages.Add(ASmartImage);
  finally
    ASmartImage.Free;
  end;
end;

{ TdxSpreadSheetInplaceEditAutoCompleteHintWindow }

function TdxSpreadSheetInplaceEditAutoCompleteHintWindow.CalcHintRect(
  MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  AIndent: Integer;
begin
  Result := cxRect(0, 0, MaxWidth, 0);
  cxScreenCanvas.Font := Font;
  cxTextOut(cxScreenCanvas.Handle, AHint, Result, CXTO_CALCRECT or DrawTextFlags);
  cxScreenCanvas.Dormant;

  AIndent := 2 * Owner.ScaleFactor.Apply(cxTextOffset);
  Inc(Result.Right, AIndent + 2);
  Inc(Result.Bottom, AIndent);
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteHintWindow.NCPaint(DC: HDC);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.FillRect(cxRectSetNullOrigin(BoundsRect),
      cxGetActualColor(Painter.GetHintBorderColor, clWindowFrame));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetInplaceEditAutoCompleteHintWindow.Paint;
var
  ATextRect: TRect;
begin
  cxPaintCanvas.BeginPaint(Canvas);
  try
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);

    cxPaintCanvas.Font := Font;
    cxPaintCanvas.Font.Color := Painter.ScreenTipGetTitleTextColor;
    cxPaintCanvas.Brush.Style := bsClear;

    ATextRect := cxRectInflate(ClientRect, -Owner.ScaleFactor.Apply(cxTextOffset));
    cxTextOut(cxPaintCanvas.Handle, Text, ATextRect, DrawTextFlags or CXTO_CENTER_VERTICALLY);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxSpreadSheetInplaceEditAutoCompleteHintWindow.GetOwnerEx: TdxSpreadSheetInplaceEditAutoCompleteWindow;
begin
  Result := inherited Owner as TdxSpreadSheetInplaceEditAutoCompleteWindow;
end;

function TdxSpreadSheetInplaceEditAutoCompleteHintWindow.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.Style.LookAndFeel.Painter;
end;

initialization

finalization
  TdxSpreadSheetInplaceEditAutoCompleteWindowImages.Finalize;
end.
