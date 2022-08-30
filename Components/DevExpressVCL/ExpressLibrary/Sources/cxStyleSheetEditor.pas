{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit cxStyleSheetEditor;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Messages, ExtDlgs,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  cxClasses, cxGraphics, cxStyles;

type
  TcxStyleSheetEditorPreview = class
  public
    constructor Create(AOwner: TComponent); virtual;
    function Control: TWinControl; virtual; abstract;
    function GetSize: TPoint; virtual;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; virtual;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); virtual; abstract;
  end;

  TcxStyleSheetEditorPreviewClass = class of TcxStyleSheetEditorPreview;

  TfrmcxStyleSheetEditor = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    bntCancel: TButton;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    pnlClient: TPanel;
    Bevel: TBevel;
    pnlStyles: TPanel;
    pnlStylesCaption: TPanel;
    pnlStylesButtons: TPanel;
    pnlStylesClient: TPanel;
    lbStyles: TListBox;
    pnlPreview: TPanel;
    pnlPreviewCaption: TPanel;
    pnlPreviewClient: TPanel;
    cbColor: TCheckBox;
    btnBitmap: TButton;
    btnColor1: TButton;
    btnFont1: TButton;
    cbFont: TCheckBox;
    cbBitmap: TCheckBox;
    procedure lbStylesClick(Sender: TObject);
    procedure lbStylesMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure lbStylesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure cbClick(Sender: TObject);
    procedure btnColor1Click(Sender: TObject);
    procedure btnFont1Click(Sender: TObject);
    procedure btnBitmapClick(Sender: TObject);
  private
    FCanvas: TcxCanvas;
    FPreview: TcxStyleSheetEditorPreview;
    FStateUpdating: Boolean;
    FStyleList: TList;
    FStyleSheet: TcxCustomStyleSheet;
    function GetCheckBoxStyleValue(ACheckBox: TCheckBox): TcxStyleValue;
    function GetFirstSelectedStyle: TcxStyle;
    procedure RecreateListBox(AListBox: TListBox);
    procedure SetSelectedStylesAssignValue(ACheckBox: TCheckBox);
    procedure SetStyles(AStyleSheet: TcxCustomStyleSheet);
    procedure UpdateStyles(AStyleSheet: TcxCustomStyleSheet; AGetStyleName: TcxStyleGetName);
    procedure UpdateState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function ShowcxStyleSheetEditor(AStyleSheet: TcxCustomStyleSheet; AGetStyleName: TcxStyleGetName): Boolean;

procedure RegisterStyleSheetEditorPreview(APreviewClass: TcxStyleSheetEditorPreviewClass);
procedure UnregisterStyleSheetEditorPreview(APreviewClass: TcxStyleSheetEditorPreviewClass);
function GetPreviewByStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass): TcxStyleSheetEditorPreviewClass;

implementation

{$R *.dfm}

uses
  TypInfo, cxControls, dxCore;

{ TcxStyleSheetEditorPreview }

constructor TcxStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create;
end;

function TcxStyleSheetEditorPreview.GetSize: TPoint;
begin
  Result.X := 350;
  Result.Y := 250;
end;

class function TcxStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := nil;
end;

function ShowcxStyleSheetEditor(AStyleSheet: TcxCustomStyleSheet;
        AGetStyleName: TcxStyleGetName): Boolean;
var
  AForm: TfrmcxStyleSheetEditor;
begin
  AForm := TfrmcxStyleSheetEditor.Create(nil);
  try
    AForm.SetStyles(AStyleSheet);
    AForm.UpdateState;
    AForm.ShowModal;
    Result := AForm.ModalResult = mrOK;
    if Result then
      AForm.UpdateStyles(AStyleSheet, AGetStyleName);
  finally
    AForm.Free;
  end;
end;

{ TfrmcxStyleSheetEditor }

constructor TfrmcxStyleSheetEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyleList := TList.Create;
  FCanvas := TcxCanvas.Create(nil);
end;

destructor TfrmcxStyleSheetEditor.Destroy;
begin
  FreeAndNil(FCanvas);
  FStyleList.Free;
  FStyleSheet.Free;
  FPreview.Free;
  inherited Destroy;
end;

function TfrmcxStyleSheetEditor.GetCheckBoxStyleValue(ACheckBox: TCheckBox): TcxStyleValue;
begin
  if ACheckBox = cbBitmap then
    Result := svBitmap
  else
    if ACheckBox = cbColor then
      Result := svColor
    else
      Result := svFont;
end;

function TfrmcxStyleSheetEditor.GetFirstSelectedStyle: TcxStyle;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to lbStyles.Items.Count - 1 do
    if lbStyles.Selected[I] then
    begin
      Result := TcxStyle(FStyleList[I]);
      Break;
    end;
end;

procedure TfrmcxStyleSheetEditor.RecreateListBox(AListBox: TListBox);
var
  I: Integer;
  ASelected: TList;
begin
  ASelected := TList.Create;
  try
    for I := 0 to AListBox.Items.Count - 1 do
      if AListBox.Selected[I] then
        ASelected.Add(Pointer(I));
    cxRecreateControlWnd(AListBox);
    for I := 0 to ASelected.Count - 1 do
      AListBox.Selected[Integer((ASelected[I]))] := True;
  finally
    ASelected.Free;
  end;
end;

procedure TfrmcxStyleSheetEditor.SetSelectedStylesAssignValue(ACheckBox: TCheckBox);

  function cxStyleValueTocxStyleValues(AStyleValue: TcxStyleValue): TcxStyleValues;
  begin
    case AStyleValue of
      svBitmap:
        Result := [svBitmap];
      svColor:
        Result := [svColor];
      svFont:
        Result := [svFont, svTextColor];
      else
        Result := [];
    end;
  end;

var
  AStyleValues: TcxStyleValues;
  I: Integer;
begin
  AStyleValues := cxStyleValueTocxStyleValues(GetCheckBoxStyleValue(ACheckBox));
  for I := 0 to lbStyles.Items.Count - 1 do
    if lbStyles.Selected[I] then
      with TcxStyle(FStyleList[I]) do
        if ACheckBox.Checked then
        begin
          if (ACheckBox = cbBitmap) and not IsGlyphAssigned(Bitmap) then
            Continue;
          AssignedValues := AssignedValues + AStyleValues
        end
        else
          AssignedValues := AssignedValues - AStyleValues;
end;

procedure TfrmcxStyleSheetEditor.SetStyles(AStyleSheet: TcxCustomStyleSheet);
var
  I, ACount: Integer;
  APropList: TPropList;
  AStyle, ACacheStyle: TcxStyle;
begin
  FStyleSheet := TcxCustomStyleSheetClass(AStyleSheet.ClassType).Create(nil);
  ACount := GetPropList(FStyleSheet.GetStyles.ClassInfo, [tkClass], @APropList);
  try
    lbStyles.Items.BeginUpdate;
    for I := 0 to ACount - 1 do
      if GetTypeData(APropList[I].PropType^).ClassType = TcxStyle then
      begin
        AStyle := TcxStyle(GetObjectProp(AStyleSheet.GetStyles, dxShortStringToString(APropList[I].Name)));
        ACacheStyle := TcxStyle.Create(Self);
        if AStyle <> nil then
          ACacheStyle.Assign(AStyle);
        SetObjectProp(FStyleSheet.GetStyles, APropList[I], ACacheStyle);
        FStyleList.Add(ACacheStyle);
        lbStyles.Items.Add(dxShortStringToString(APropList[I].Name));
      end;
  finally
    lbStyles.Items.EndUpdate;
  end;
  if GetPreviewByStyleSheetClass(TcxCustomStyleSheetClass(FStyleSheet.ClassType)) <> nil then
  begin
    FPreview := GetPreviewByStyleSheetClass(TcxCustomStyleSheetClass(FStyleSheet.ClassType)).Create(self);
    FPreview.Control.Parent := pnlPreviewClient;
    FPreview.Control.Align := alClient;
    FPreview.SetStyleSheet(FStyleSheet);
    Width := pnlStyles.Width + lbStyles.Left * 2 + FPreview.GetSize.X;
    if pnlPreviewClient.Height < FPreview.GetSize.Y then
      Height := Height + FPreview.GetSize.Y - pnlPreviewClient.Height;
  end else
  begin
    pnlPreview.Visible := False;
    Width := pnlStyles.Width + lbStyles.Left * 2;
    FPreview := nil;
  end;
end;

procedure TfrmcxStyleSheetEditor.UpdateStyles(AStyleSheet: TcxCustomStyleSheet;
        AGetStyleName: TcxStyleGetName);
begin
  CreateStyleSheetStyles(AStyleSheet, FStyleSheet, AGetStyleName)
end;

procedure TfrmcxStyleSheetEditor.UpdateState;

  procedure UpdateCheckBox(ACheckBox: TCheckBox; AStyleValue: TcxStyleValue);

    function SelectedStylesHaveSameAssignedValue: Boolean;
    var
      AIsStyleValueAssigned: Boolean;
      I: Integer;
    begin
      AIsStyleValueAssigned := AStyleValue in GetFirstSelectedStyle.AssignedValues;
      Result := True;
      for I := 0 to lbStyles.Items.Count - 1 do
        if lbStyles.Selected[I] and ((AStyleValue in TcxStyle(FStyleList[I]).AssignedValues) <> AIsStyleValueAssigned) then
        begin
          Result := False;
          Break;
        end;
    end;

    function GetCheckBoxState: TCheckBoxState;
    const
      ACheckBoxStateMap: array [Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
    begin
      Result := cbUnchecked;
      if ACheckBox.Enabled then
        if SelectedStylesHaveSameAssignedValue then
          Result := ACheckBoxStateMap[AStyleValue in GetFirstSelectedStyle.AssignedValues]
        else
          Result := cbGrayed
    end;

  begin
    ACheckBox.Enabled := lbStyles.SelCount > 0;
    ACheckBox.State := GetCheckBoxState;
  end;

begin
  FStateUpdating := True;
  try
    btnBitmap.Enabled := lbStyles.SelCount > 0;
    btnColor1.Enabled := btnBitmap.Enabled;
    btnFont1.Enabled := btnBitmap.Enabled;

    UpdateCheckBox(cbBitmap, svBitmap);
    UpdateCheckBox(cbColor, svColor);
    UpdateCheckBox(cbFont, svFont);
  finally
    FStateUpdating := False;
  end;
end;

procedure TfrmcxStyleSheetEditor.lbStylesClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TfrmcxStyleSheetEditor.lbStylesMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  AcxCanvas: TcxCanvas;
begin
  if Index >= FStyleList.Count then exit;
  if (svFont in TcxStyle(FStyleList[Index]).AssignedValues) then
  begin
    AcxCanvas := TcxCanvas.Create(lbStyles.Canvas);
    try
      AcxCanvas.Font.Assign(TcxStyle(FStyleList[Index]).Font);
      Height := AcxCanvas.TextHeight(lbStyles.Items[Index]) + 2;
    finally
      AcxCanvas.Free;
    end;
  end;
end;

procedure TfrmcxStyleSheetEditor.lbStylesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AFlags: Longint;
  ASelected: Boolean;
  AStyle: TcxStyle;
  AText: string;
  APrevTextColor: TColor;
begin
  ASelected := odSelected in State;
  AStyle := TcxStyle(FStyleList[Index]);

  if not ASelected and (svColor in AStyle.AssignedValues) then
    lbStyles.Canvas.Brush.Color := AStyle.Color;
  if not ASelected and (svBitmap in AStyle.AssignedValues) then
  begin
    FCanvas.Canvas := lbStyles.Canvas;
    FCanvas.FillRect(Rect, AStyle.Bitmap);
  end
  else
    lbStyles.Canvas.FillRect(Rect);

  AFlags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  Inc(Rect.Left, 2);
  AText := lbStyles.Items[Index];
  if (svFont in AStyle.AssignedValues) then
  begin
    APrevTextColor := lbStyles.Canvas.Font.Color;
    lbStyles.Canvas.Font.Assign(AStyle.Font);
    lbStyles.Canvas.Font.Color := APrevTextColor;
  end;
  if (svTextColor in AStyle.AssignedValues) and not ASelected then
    lbStyles.Canvas.Font.Color := AStyle.TextColor;
  lbStyles.Canvas.Brush.Style := bsClear;
  DrawText(lbStyles.Canvas.Handle, PChar(AText), Length(AText), Rect, AFlags);
  lbStyles.Canvas.Brush.Style := bsSolid;
end;

var
  FStyleSheetPreviews: TList = nil;

procedure RegisterStyleSheetEditorPreview(APreviewClass: TcxStyleSheetEditorPreviewClass);
begin
  if FStyleSheetPreviews = nil then
    FStyleSheetPreviews := TList.Create;
  if FStyleSheetPreviews.IndexOf(TObject(APreviewClass)) < 0 then
    FStyleSheetPreviews.Add(TObject(APreviewClass));
end;

procedure UnregisterStyleSheetEditorPreview(APreviewClass: TcxStyleSheetEditorPreviewClass);
begin
  if FStyleSheetPreviews <> nil then
    FStyleSheetPreviews.Remove(TObject(APreviewClass));
end;

function GetPreviewByStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass): TcxStyleSheetEditorPreviewClass;
var
  I: Integer;
begin
  Result := nil;
  if FStyleSheetPreviews <> nil then
    for I := 0 to FStyleSheetPreviews.Count - 1 do
      if (TcxStyleSheetEditorPreviewClass(FStyleSheetPreviews[I]).GetStyleSheetClass = AStyleSheetClass) then
      begin
        Result := TcxStyleSheetEditorPreviewClass(FStyleSheetPreviews[I]);
        Break;
      end;
end;

procedure TfrmcxStyleSheetEditor.cbClick(Sender: TObject);
begin
  if not FStateUpdating then
    try
      SetSelectedStylesAssignValue(TCheckBox(Sender));
    finally
      if Sender = cbFont then
        RecreateListBox(lbStyles)
      else
        lbStyles.Invalidate;
      UpdateState;
    end;
end;

procedure TfrmcxStyleSheetEditor.btnColor1Click(Sender: TObject);
var
  I: Integer;
begin
  ColorDialog.Color := TcxStyle(FStyleList[lbStyles.ItemIndex]).Color;
  if ColorDialog.Execute then
    try
      for I := 0 to lbStyles.Items.Count - 1 do
        if lbStyles.Selected[I] then
          TcxStyle(FStyleList[I]).Color := ColorDialog.Color;
    finally
      lbStyles.Invalidate;
      UpdateState;
    end;
end;

procedure TfrmcxStyleSheetEditor.btnFont1Click(Sender: TObject);
var
  I: Integer;
begin
  FontDialog.Font.Assign(TcxStyle(FStyleList[lbStyles.ItemIndex]).Font);
  FontDialog.Font.Color := TcxStyle(FStyleList[lbStyles.ItemIndex]).TextColor;
  if FontDialog.Execute then
    try
      for I := 0 to lbStyles.Items.Count - 1 do
        if lbStyles.Selected[I] then
        begin
          TcxStyle(FStyleList[I]).Font.Assign(FontDialog.Font);
          TcxStyle(FStyleList[I]).TextColor := FontDialog.Font.Color;
        end;
    finally
      RecreateListBox(lbStyles);
      UpdateState;
    end;
end;

procedure TfrmcxStyleSheetEditor.btnBitmapClick(Sender: TObject);
var
  I: Integer;
  AOpenPictureDialog: TOpenPictureDialog;
begin
  AOpenPictureDialog := TOpenPictureDialog.Create(nil);
  try
    with AOpenPictureDialog do
    begin
      Filter := 'Bitmaps (*.bmp)|*.bmp';
      Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
      FileName := '';
    end;
    if AOpenPictureDialog.Execute then
      try
        for I := 0 to lbStyles.Items.Count - 1 do
          if lbStyles.Selected[I] then
            TcxStyle(FStyleList[I]).Bitmap.LoadFromFile(AOpenPictureDialog.FileName);
      finally
        lbStyles.Invalidate;
        UpdateState;
      end;
  finally
    AOpenPictureDialog.Free;
  end;
end;

initialization

finalization
  FStyleSheetPreviews.Free;
  FStyleSheetPreviews := nil;
end.
