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

unit cxImageListEditorView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, CommCtrl, Menus, ImgList, ToolWin, cxGraphics,
  cxClasses, cxImageListEditor, ActnList, Dialogs, ExtDlgs, cxImageList, dxGDIPlusClasses;

type
  TcxImageListEditorFormInternalState = (eisSelectingTransparentColor);
  TcxImageListEditorFormInternalStates = set of TcxImageListEditorFormInternalState;

  TcxImageListEditorForm = class(TcxCustomImageListEditorForm)
    actAdd: TAction;
    actAddFromDXGallery: TAction;
    actAddFromFile: TAction;
    actApply: TAction;
    actClear: TAction;
    actConvertTo32bit: TAction;
    actDelete: TAction;
    actExport: TAction;
    actExportAsBitmap: TAction;
    actExportAsPNG: TAction;
    actImport: TAction;
    actInsert: TAction;
    actlCommands: TActionList;
    actOK: TAction;
    actReplace: TAction;
    actReplaceFromDXGallery: TAction;
    actReplaceFromFile: TAction;
    AddFromDXGallery1: TMenuItem;
    AddFromFile1: TMenuItem;
    AsBitmap1: TMenuItem;
    AsBitmap2: TMenuItem;
    AsPNG1: TMenuItem;
    AsPNG2: TMenuItem;
    btnApply: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    cbBackgroundFill: TComboBox;
    cbGridlines: TCheckBox;
    cbImagesSize: TComboBox;
    cbTransparentColor: TComboBox;
    gbImages: TGroupBox;
    gbSelectedImage: TGroupBox;
    imglSmall: TcxImageList;
    imgWarning: TImage;
    lbBackgroundFill: TLabel;
    lblManifestWarning: TLabel;
    lbTransparentColor: TLabel;
    lvImages: TListView;
    miAddFromDXGallery: TMenuItem;
    miAddFromFile: TMenuItem;
    miClear: TMenuItem;
    miConvertTo32bit: TMenuItem;
    miDelete: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miReplaceFromDevExpressGallery: TMenuItem;
    miReplaceFromDisk: TMenuItem;
    opdOpen: TOpenPictureDialog;
    pbPreview: TPaintBox;
    pmAdd: TPopupMenu;
    pmCommands: TPopupMenu;
    pmExport: TPopupMenu;
    pmImageLists: TPopupMenu;
    pmReplace: TPopupMenu;
    pnlBottom: TPanel;
    pnlBottomBar: TPanel;
    pnlClient: TPanel;
    pnlToolBarSubstrate: TPanel;
    ReplaceFromDXGallery1: TMenuItem;
    ReplaceFromFile1: TMenuItem;
    spdSave: TSavePictureDialog;
    tbbAdd: TToolButton;
    tbbClear: TToolButton;
    tbbConvertTo32bit: TToolButton;
    tbbDelete: TToolButton;
    tbbExport: TToolButton;
    tbbImport: TToolButton;
    tbbReplace: TToolButton;
    tbCommands: TToolBar;
    tbbSeparator: TToolButton;
    tbbShowImageTypes: TToolButton;
    actExportAsSVG: TAction;
    asSVG1: TMenuItem;
    asSVG2: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure cbImagesSizeChange(Sender: TObject);
    procedure cbTransparentColorChange(Sender: TObject);
    procedure cbTransparentColorExit(Sender: TObject);
    procedure lvImagesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvImagesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvImagesEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lvImagesStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure lvImagesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure pbPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPreviewPaint(Sender: TObject);

    procedure actAddExecute(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actExportAsBitmapExecute(Sender: TObject);
    procedure actExportAsPNGExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actConvertTo32bitExecute(Sender: TObject);
    procedure actAddFromFileExecute(Sender: TObject);
    procedure actAddFromDXGalleryExecute(Sender: TObject);
    procedure actReplaceFromFileExecute(Sender: TObject);
    procedure actReplaceFromDXGalleryExecute(Sender: TObject);
    procedure cbBackgroundFillChange(Sender: TObject);
    procedure cbBackgroundFillExit(Sender: TObject);
    procedure cbGridlinesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tbbShowImageTypesClick(Sender: TObject);
    procedure actExportAsSVGExecute(Sender: TObject);
    procedure actExportAsSVGUpdate(Sender: TObject);
  private
    FImportList: TStrings;
    FDragImageIndex: Integer;
    FPreviewImageList: TcxImageList;
    FInternalState: TcxImageListEditorFormInternalStates;

    procedure AddColor(const AColor: string);
    procedure ChangeImagesSize;
    procedure DrawFocusedItem(ACanvas: TCanvas; ARect: TRect);
    procedure DrawGridlines;
    function ExtractImagesSize(const ASizeAsText: string): TSize;
    function GetColorFromCursorPos(X, Y: Integer): TColor;
    function GetFocusedImageIndex: Integer;
    function GetFocusedImageInfo: TcxImageInfo;
    procedure SetFocusedImageIndex(AValue: Integer);

    procedure AddImages(AAddMode: TcxImageListEditorAddMode);
    procedure AddImagesFromDXGallery(AAddMode: TcxImageListEditorAddMode);
    procedure ImportImageList(Sender: TObject);
    procedure PopulateImportItems;

    procedure UpdateActions; reintroduce;
    procedure UpdateImagesSizeIndicator;
    procedure UpdateTransparentColor(AColor: TColor); overload;
    procedure UpdateTransparentColor(X, Y: Integer); overload;
    procedure UpdateTransparentColorIndicator(AColor: TColor);
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure UpdateControls; override;
    procedure RestorePosition; override;
    procedure StorePosition; override;

    property FocusedImageIndex: Integer read GetFocusedImageIndex write SetFocusedImageIndex;
    property FocusedImageInfo: TcxImageInfo read GetFocusedImageInfo;
  public
    constructor Create(AImageListEditor: TcxImageListEditor); override;
    destructor Destroy; override;

    function GetVisualDataControl: TListView; override;
    procedure SetImportList(AValue: TStrings); override;
  end;

implementation

{$R *.dfm}

uses
  Types, Math, cxGeometry, cxControls, dxOffice11, cxLibraryConsts, ShellApi, dxIconLibraryEditorHelpers, dxSmartImage;

const
  sMsgErrorInvalidImageHeight = 'The specified height value is incorrect';
  sMsgErrorInvalidImageWidth = 'The specified width value is incorrect';

type
  TcxImageListAccess = class(TcxImageList);

var
  dxvEditorFormPosition: TRect;
  dxcbBackgroundFill: Integer;
  dxcbGridLines: Boolean;
  dxcbShowImageTypes: Boolean;

{ TcxImageListEditorForm }

constructor TcxImageListEditorForm.Create(AImageListEditor: TcxImageListEditor);
begin
  inherited;
  PopupMode := pmAuto;
  FPreviewImageList := TcxImageList.Create(Self);
  if IsXPManifestEnabled then
  begin
    imgWarning.Visible := True;
    lblManifestWarning.Caption := 'These images may be distorted if used in standard Windows UI controls with XPManifest enabled.';
    lblManifestWarning.Visible := True;
    Width := Width + cxGetValueCurrentDPI(6{Rows} * 3{Pixel});
  end;
end;

destructor TcxImageListEditorForm.Destroy;
begin
  FreeAndNil(FPreviewImageList);
  inherited;
end;

function TcxImageListEditorForm.GetVisualDataControl: TListView;
begin
  Result := lvImages;
end;

procedure TcxImageListEditorForm.SetImportList(AValue: TStrings);
begin
  FImportList := AValue;
  PopulateImportItems;
end;

procedure TcxImageListEditorForm.FormCreate(Sender: TObject);
begin
  lvImages.OnChange := lvImagesChange;

  pbPreview.Cursor := crcxColorPicker;
  cbBackgroundFill.Items.Add('Checkered pattern');
  GetColorValues(AddColor);
  FDragImageIndex := -1;
  cbBackgroundFill.ItemIndex := 0;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  DragAcceptFiles(Handle, True);

  if Supports(dxIconLibraryIntf, IdxImageCollectionEditor) then
  begin
    tbbAdd.Action := actAdd;
    tbbAdd.DropdownMenu := pmAdd;
    tbbReplace.Action := actReplace;
    tbbReplace.DropdownMenu := pmReplace;
    actAddFromDXGallery.Visible := True;
    actReplaceFromDXGallery.Visible := True;
  end
  else
  begin
    tbbAdd.Action := actAddFromFile;
    tbbAdd.DropdownMenu := nil;
    tbbReplace.Action := actReplaceFromFile;
    tbbReplace.DropdownMenu := nil;
    actAddFromDXGallery.Visible := False;
    actReplaceFromDXGallery.Visible := False;
  end;
end;

procedure TcxImageListEditorForm.FormDestroy(Sender: TObject);
begin
  lvImages.OnChange := nil;
end;

procedure TcxImageListEditorForm.FormHide(Sender: TObject);
begin
  dxcbShowImageTypes := tbbShowImageTypes.Down;
  dxcbGridLines := cbGridlines.Checked;
  dxcbBackgroundFill := cbBackgroundFill.ItemIndex;
end;

procedure TcxImageListEditorForm.FormShow(Sender: TObject);
begin
  if not cxRectIsEqual(dxvEditorFormPosition, cxNullRect) then
  begin
    cbBackgroundFill.ItemIndex := dxcbBackgroundFill;
    cbGridlines.Checked := dxcbGridLines;
    tbbShowImageTypes.Down := dxcbShowImageTypes;
    tbbShowImageTypesClick(nil);
  end;
end;

procedure TcxImageListEditorForm.cbBackgroundFillChange(Sender: TObject);
begin
  if cbBackgroundFill.ItemIndex <> -1 then
    UpdateControls;
end;

procedure TcxImageListEditorForm.cbBackgroundFillExit(Sender: TObject);
begin
  UpdateControls;
  if cbBackgroundFill.ItemIndex <> 0 then
    cbBackgroundFill.Text := ColorToString(StringToColor(cbBackgroundFill.Text));
end;

procedure TcxImageListEditorForm.cbGridlinesClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TcxImageListEditorForm.cbImagesSizeChange(Sender: TObject);
begin
  ChangeImagesSize;
  UpdateImagesSizeIndicator;
end;

procedure TcxImageListEditorForm.cbTransparentColorChange(Sender: TObject);
begin
  if cbTransparentColor.Items.IndexOf(cbTransparentColor.Text) <> -1 then
    UpdateTransparentColor(StringToColor(cbTransparentColor.Text));
end;

procedure TcxImageListEditorForm.cbTransparentColorExit(Sender: TObject);
begin
  UpdateTransparentColor(StringToColor(cbTransparentColor.Text));
end;

procedure TcxImageListEditorForm.lvImagesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  UpdateControls;
end;

procedure TcxImageListEditorForm.lvImagesDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := lvImages.GetItemAt(X, Y) <> nil;
end;

procedure TcxImageListEditorForm.lvImagesEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  ATargetItem: TListItem;
begin
  FImageListEditor.EndUpdate;
  ATargetItem := lvImages.GetItemAt(X, Y);
  if ATargetItem <> nil then
    FImageListEditor.MoveImage(FDragImageIndex, ATargetItem.ImageIndex)
  else
    FocusedImageIndex := FDragImageIndex;
  FDragImageIndex := -1;
end;

procedure TcxImageListEditorForm.lvImagesStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FImageListEditor.BeginUpdate;
  FDragImageIndex := FImageListEditor.FocusedImageIndex;
end;

procedure TcxImageListEditorForm.lvImagesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    FImageListEditor.DeleteSelectedImages;
end;

procedure TcxImageListEditorForm.pbPreviewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Include(FInternalState, eisSelectingTransparentColor);
    UpdateTransparentColor(X, Y);
  end;
end;

procedure TcxImageListEditorForm.pbPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if eisSelectingTransparentColor in FInternalState then
    UpdateTransparentColor(X, Y);
end;

procedure TcxImageListEditorForm.pbPreviewMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Exclude(FInternalState, eisSelectingTransparentColor);
end;

procedure TcxImageListEditorForm.pbPreviewPaint(Sender: TObject);
var
  ARect: TRect;
begin
  ARect := pbPreview.ClientRect;
  FrameRectByColor(pbPreview.Canvas.Handle, ARect, clNavy);
  InflateRect(ARect, -1, -1);
  if (cbBackgroundFill.Text = 'clNone') or (cbBackgroundFill.ItemIndex = 0) then
    cxDrawTransparencyCheckerboard(pbPreview.Canvas.Handle, ARect)
  else
  begin
    pbPreview.Canvas.Brush.Color := StringToColor(cbBackgroundFill.Text);
    pbPreview.Canvas.FillRect(ARect);
  end;
  DrawFocusedItem(pbPreview.Canvas, ARect);
  if cbGridlines.Checked then
    DrawGridlines;
end;

procedure TcxImageListEditorForm.AddColor(const AColor: string);
begin
  cbTransparentColor.Items.Add(AColor);
  cbBackgroundFill.Items.Add(AColor);
end;

procedure TcxImageListEditorForm.ChangeImagesSize;
var
  ASizeAsText: string;
{$IFDEF DELPHIXE2}
  AValues: array of string;
{$ENDIF}
begin
  if cbImagesSize.ItemIndex = 0 then
  begin
  {$IFDEF DELPHIXE2}
    SetLength(AValues, 2);
    AValues[0] := IntToStr(FImageListEditor.ImageHeight);
    AValues[1] := IntToStr(FImageListEditor.ImageWidth);
  {$ELSE}
    ASizeAsText := Format('%dx%d', [FImageListEditor.ImageWidth, FImageListEditor.ImageHeight]);
  {$ENDIF}
    while True do
    begin
    {$IFDEF DELPHIXE2}
      if InputQuery('Custom Size', ['Height:', 'Width:'], AValues) then
        ASizeAsText := AValues[1] + 'x' + AValues[0]
      else
        Exit;
    {$ELSE}
      if not InputQuery('Custom Size', 'Width x Height:', ASizeAsText) then
        Exit;
    {$ENDIF}
      try
        ExtractImagesSize(ASizeAsText);
        Break;
      except
        on E: Exception do
          MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  end
  else
    ASizeAsText := cbImagesSize.Text;

  FImageListEditor.ChangeImagesSize(ExtractImagesSize(ASizeAsText));
end;

procedure TcxImageListEditorForm.DrawFocusedItem(ACanvas: TCanvas; ARect: TRect);
begin
  if FImageListEditor.IsAnyImageSelected then
  begin
    FPreviewImageList.Width := FImageListEditor.ImageWidth;
    FPreviewImageList.Height := FImageListEditor.ImageHeight;
    TcxImageListAccess(FPreviewImageList).AddImageInfo(FocusedImageInfo);
    FPreviewImageList.Draw(ACanvas, ARect, 0);
    FPreviewImageList.Clear;
  end;
end;

procedure TcxImageListEditorForm.DrawGridlines;
var
  I: Integer;
  R: TRect;
begin
  R := cxRectInflate(pbPreview.ClientRect, -1, -1);
  for I := 1 to FImageListEditor.ImageWidth - 1 do
    cxFillHalfToneRect(pbPreview.Canvas, cxRectSetLeft(R,
      MulDiv(I, pbPreview.Width - 1, FImageListEditor.ImageWidth), 1), $808080, $C0C0C0);
  for I := 1 to FImageListEditor.ImageHeight - 1 do
    cxFillHalfToneRect(pbPreview.Canvas, cxRectSetTop(R,
      MulDiv(I, pbPreview.Height - 1, FImageListEditor.ImageHeight), 1), $808080, $C0C0C0);
end;

function TcxImageListEditorForm.GetColorFromCursorPos(X, Y: Integer): TColor;
begin
  if cxRectPtIn(Rect(0, 0, pbPreview.Width, pbPreview.Height), X, Y) then
  begin
    X := X * FImageListEditor.ImageWidth div pbPreview.Width;
    Y := Y * FImageListEditor.ImageHeight div pbPreview.Height;
    Result := TcxImageInfoHelper.GetPixel(FocusedImageInfo.Image, X, Y);
  end
  else
    Result := FocusedImageInfo.MaskColor;
end;

function TcxImageListEditorForm.GetFocusedImageIndex: Integer;
begin
  Result := FImageListEditor.FocusedImageIndex;
end;

function TcxImageListEditorForm.GetFocusedImageInfo: TcxImageInfo;
begin
  Result := FImageListEditor.ImagesInfo[FocusedImageIndex];
end;

function TcxImageListEditorForm.ExtractImagesSize(const ASizeAsText: string): TSize;
var
  APosition: Integer;
begin
  APosition := Pos('x', ASizeAsText);
  if (APosition = 0) or not TryStrToInt(Trim(Copy(ASizeAsText, 1, APosition - 1)), Result.cx) or (Result.cx <= 0) then
    raise EInvalidArgument.Create(sMsgErrorInvalidImageWidth);
  if (APosition = 0) or not TryStrToInt(Trim(Copy(ASizeAsText, APosition + 1, MaxInt)), Result.cy) or (Result.cy <= 0) then
    raise EInvalidArgument.Create(sMsgErrorInvalidImageHeight);
end;

procedure TcxImageListEditorForm.SetFocusedImageIndex(AValue: Integer);
begin
  FImageListEditor.FocusedImageIndex := AValue;
end;

procedure TcxImageListEditorForm.AddImages(AAddMode: TcxImageListEditorAddMode);
begin
  opdOpen.Filter := cxImageFileFormats.GetFilter;
  if opdOpen.Execute then
    FImageListEditor.AddImages(opdOpen.Files, AAddMode);
end;

procedure TcxImageListEditorForm.AddImagesFromDXGallery(AAddMode: TcxImageListEditorAddMode);
var
  AFiles: TStrings;
  AImageCollectionEditor: IdxImageCollectionEditor;
begin
  if Supports(dxIconLibraryIntf, IdxImageCollectionEditor, AImageCollectionEditor) then
  begin
    AFiles := TStringList.Create;
    try
      if AImageCollectionEditor.Execute(AFiles, cxSize(FImageListEditor.ImageWidth, FImageListEditor.ImageHeight)) then
        FImageListEditor.AddImages(AFiles, AAddMode);
    finally
      AFiles.Free;
    end;
  end;
end;

procedure TcxImageListEditorForm.ImportImageList(Sender: TObject);
begin
  FImageListEditor.ImportImages(FImportList.Objects[TMenuItem(Sender).Tag] as TCustomImageList);
end;

procedure TcxImageListEditorForm.PopulateImportItems;

  procedure PopulateItem(AParentItem: TMenuItem; const APrefix: string);
  var
    AMenuItem: TMenuItem;
    I: Integer;
  begin
    AParentItem.Clear;
    for I := 0 to FImportList.Count - 1 do
    begin
      AMenuItem := TMenuItem.Create(Self);
      AMenuItem.OnClick := ImportImageList;
      AMenuItem.Caption := APrefix + FImportList[I];
      AMenuItem.Tag := I;
      AMenuItem.ImageIndex := 5;
      AParentItem.Add(AMenuItem);
    end;
  end;

begin
  PopulateItem(pmImageLists.Items, 'Import from ');
  PopulateItem(miImport, 'from ');
end;

procedure TcxImageListEditorForm.UpdateActions;
begin
  actDelete.Enabled := FImageListEditor.IsAnyImageSelected;
  actClear.Enabled := FImageListEditor.ImagesCount > 0;
  actExport.Enabled := FImageListEditor.ImagesCount > 0;
  actExportAsBitmap.Enabled := actExport.Enabled;
  actExportAsPNG.Enabled := actExport.Enabled;
  actReplace.Enabled := FImageListEditor.IsAnyImageSelected;
  actReplaceFromFile.Enabled := actReplace.Enabled;
  actReplaceFromDXGallery.Enabled := actReplace.Enabled;
  actInsert.Enabled := FImageListEditor.IsAnyImageSelected;
  actApply.Enabled := FImageListEditor.IsChanged;
  actImport.Enabled := (FImportList <> nil) and (FImportList.Count <> 0);
  actConvertTo32bit.Enabled := FImageListEditor.ImagesCount > 0;
end;

procedure TcxImageListEditorForm.UpdateControls;
var
  AAllowSelectTransparentColor: Boolean;
begin
  if FImageListEditor.IsUpdateLocked then
    Exit;

  cbGridlines.Enabled := (FImageListEditor.ImageWidth <= 48) or (FImageListEditor.ImageWidth <= 48);
  cbGridlines.Checked := cbGridlines.Checked and cbGridlines.Enabled;

  AAllowSelectTransparentColor := FImageListEditor.IsAnyImageSelected and
    not (IsGlyphAssigned(FocusedImageInfo.Mask) or FocusedImageInfo.IsAlphaUsed);

  //gbSelectedImage
  pbPreview.Enabled := AAllowSelectTransparentColor;
  cbTransparentColor.Enabled := AAllowSelectTransparentColor;
  lbTransparentColor.Enabled := AAllowSelectTransparentColor;

  pbPreview.Invalidate;

  if AAllowSelectTransparentColor then
    UpdateTransparentColorIndicator(FocusedImageInfo.MaskColor)
  else
    UpdateTransparentColorIndicator(clNone);

  UpdateImagesSizeIndicator;
  UpdateActions;
end;

procedure TcxImageListEditorForm.RestorePosition;
begin
  if not cxRectIsEqual(dxvEditorFormPosition, cxNullRect) then
    SetBounds(dxvEditorFormPosition.Left, dxvEditorFormPosition.Top, dxvEditorFormPosition.Right, dxvEditorFormPosition.Bottom);
end;

procedure TcxImageListEditorForm.StorePosition;
begin
  dxvEditorFormPosition := Rect(Left, Top, Width, Height);
end;

procedure TcxImageListEditorForm.tbbShowImageTypesClick(Sender: TObject);
begin
  FImageListEditor.ShowImageTypes := tbbShowImageTypes.Down;
end;

procedure TcxImageListEditorForm.UpdateImagesSizeIndicator;
var
  AImagesSizeDisplayText: string;
  ASizeIndex: Integer;
begin
  AImagesSizeDisplayText := Format('%dx%d', [FImageListEditor.ImageWidth, FImageListEditor.ImageHeight]);
  ASizeIndex := cbImagesSize.Items.IndexOf(AImagesSizeDisplayText);
  if ASizeIndex <> -1 then
    cbImagesSize.ItemIndex := ASizeIndex
  else
    cbImagesSize.Items.Add(AImagesSizeDisplayText);
end;

procedure TcxImageListEditorForm.UpdateTransparentColor(AColor: TColor);
begin
  FImageListEditor.UpdateTransparentColor(AColor);
end;

procedure TcxImageListEditorForm.UpdateTransparentColor(X, Y: Integer);
begin
  UpdateTransparentColor(GetColorFromCursorPos(X, Y));
end;

procedure TcxImageListEditorForm.UpdateTransparentColorIndicator(AColor: TColor);
begin
  cbTransparentColor.Text := ColorToString(AColor);
end;

procedure TcxImageListEditorForm.WMDropFiles(var Message: TWMDropFiles);
var
  I, ACount: Integer;
  AFileName: array [0..MAX_PATH] of Char;
  AFiles: TStringList;
begin
  inherited;
  AFiles := TStringList.Create;
  try
    try
      ACount := DragQueryFile(Message.Drop, $FFFFFFFF, AFileName, MAX_PATH);
      for I := 0 to ACount - 1 do
      begin
        DragQueryFile(Message.Drop, I, AFileName, MAX_PATH);
        AFiles.Add(AFileName);
      end;
    finally
      DragFinish(Message.Drop);
    end;
    if AFiles.Count > 0 then
      FImageListEditor.AddImages(AFiles, amAdd);
  finally
    AFiles.Free;
  end;
end;

procedure TcxImageListEditorForm.actAddExecute(Sender: TObject);
begin
  // (don't remove this method)
end;

procedure TcxImageListEditorForm.actInsertExecute(Sender: TObject);
begin
  AddImages(amInsert);
end;

procedure TcxImageListEditorForm.actReplaceExecute(Sender: TObject);
begin
  // (don't remove this method)
end;

procedure TcxImageListEditorForm.actDeleteExecute(Sender: TObject);
begin
  FImageListEditor.DeleteSelectedImages;
end;

procedure TcxImageListEditorForm.actClearExecute(Sender: TObject);
begin
  FImageListEditor.ClearImages;
end;

procedure TcxImageListEditorForm.actImportExecute(Sender: TObject);
begin
  // (don't remove this method)
end;

procedure TcxImageListEditorForm.actApplyExecute(Sender: TObject);
begin
  FImageListEditor.ApplyChanges;
end;

procedure TcxImageListEditorForm.actOKExecute(Sender: TObject);
begin
  FImageListEditor.ApplyChanges;
end;

procedure TcxImageListEditorForm.actExportAsBitmapExecute(Sender: TObject);
begin
  spdSave.DefaultExt := '*.bmp';
  spdSave.Filter := 'Bitmaps (*.bmp)|*.bmp';
  if spdSave.Execute then
    FImageListEditor.ExportImages(spdSave.FileName, itBitmap);
end;

procedure TcxImageListEditorForm.actExportAsPNGExecute(Sender: TObject);
begin
  spdSave.DefaultExt := '*.png';
  spdSave.Filter := 'PNG (*.png)|*.png';
  if spdSave.Execute then
    FImageListEditor.ExportImages(spdSave.FileName, itPNG);
end;

procedure TcxImageListEditorForm.actExportAsSVGExecute(Sender: TObject);
begin
  spdSave.DefaultExt := '*.svg';
  spdSave.Filter := 'SVG (*.svg)|*.svg';
  if spdSave.Execute then
    FocusedImageInfo.Image.SaveToFile(spdSave.FileName);
end;

procedure TcxImageListEditorForm.actExportAsSVGUpdate(Sender: TObject);
begin
  actExportAsSVG.Visible := (FImageListEditor.GetSelectionCount = 1) and Supports(FocusedImageInfo.Image, IdxVectorImage);
end;

procedure TcxImageListEditorForm.actExportExecute(Sender: TObject);
begin
  // (don't remove this method)
end;

procedure TcxImageListEditorForm.actConvertTo32bitExecute(Sender: TObject);
begin
  FImageListEditor.ConvertTo32bit;
end;

procedure TcxImageListEditorForm.actAddFromFileExecute(Sender: TObject);
begin
  AddImages(amAdd);
end;

procedure TcxImageListEditorForm.actAddFromDXGalleryExecute(Sender: TObject);
begin
  AddImagesFromDXGallery(amAdd);
end;

procedure TcxImageListEditorForm.actReplaceFromFileExecute(Sender: TObject);
begin
  AddImages(amReplace);
end;

procedure TcxImageListEditorForm.actReplaceFromDXGalleryExecute(Sender: TObject);
begin
  AddImagesFromDXGallery(amReplace);
end;

end.

