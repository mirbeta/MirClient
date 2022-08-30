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

unit dxScreenTipRepositoryEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Types, Dialogs, cxComponentCollectionEditor, Menus, ActnList, ImgList, ComCtrls,
  dxScreenTip, dxCustomHint, ToolWin, ExtCtrls, Buttons, StdCtrls,
  cxDesignWindows, ExtDlgs, StrUtils, DesignIntf, StdActns, cxGraphics;

type
  TfrmScreenTipRepositoryEditor = class(TfrmComponentCollectionEditor)
    pnlButtons: TPanel;
    btnClose: TButton;
    pnlPreviewAndOptions: TPanel;
    pnlPreview: TPanel;
    gbPreview: TGroupBox;
    PaintBox1: TPaintBox;
    pnlOptions: TPanel;
    pcOptions: TPageControl;
    tsScreenTipOptions: TTabSheet;
    gbScreenTipInfo: TGroupBox;
    Label1: TLabel;
    edtHeader: TEdit;
    edtDescription: TMemo;
    edtFooter: TEdit;
    chbUseHintAsHeader: TCheckBox;
    chbUseStandardFooter: TCheckBox;
    UpDown1: TUpDown;
    edtWidth: TEdit;
    Panel1: TPanel;
    pbDescription: TPaintBox;
    Panel2: TPanel;
    pbFooter: TPaintBox;
    Panel3: TPanel;
    pbHeader: TPaintBox;
    tsRepositoryOptions: TTabSheet;
    gbScreenTipsInfo: TGroupBox;
    Label2: TLabel;
    btnHeaderFont: TSpeedButton;
    btnDescriptionFont: TSpeedButton;
    btnFooterFont: TSpeedButton;
    lblHeaderFontInfo: TLabel;
    lblDescriptionFontInfo: TLabel;
    lblFooterFontInfo: TLabel;
    chbShowDescription: TCheckBox;
    edtStandardFooter: TEdit;
    Panel4: TPanel;
    pbStandardFooter: TPaintBox;
    chbHeaderFont: TCheckBox;
    chbDescriptionFont: TCheckBox;
    chbFooterFont: TCheckBox;
    Splitter1: TSplitter;
    actLeftDescriptionAlign: TAction;
    OpenPictureDialog1: TOpenPictureDialog;
    PopupMenu3: TPopupMenu;
    Open1: TMenuItem;
    Clear1: TMenuItem;
    Align1: TMenuItem;
    Left1: TMenuItem;
    Right1: TMenuItem;
    FontDialog1: TFontDialog;
    FixedWidth1: TMenuItem;
    btnHeaderRichTextEdit: TSpeedButton;
    btnDescriptionRichTextEdit: TSpeedButton;
    btnFooterRichTextEdit: TSpeedButton;
    btnStdFooterRichTextEdit: TSpeedButton;
    chbPlainText: TCheckBox;
    chbHeaderPlain: TCheckBox;
    chbDescriptionPlain: TCheckBox;
    chbFooterPlain: TCheckBox;
    btnShowOptions: TButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure edtHeaderChange(Sender: TObject);
    procedure edtStandardFooterChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure chbShowDescriptionClick(Sender: TObject);
    procedure chbHeaderFontClick(Sender: TObject);
    procedure btnFooterFontClick(Sender: TObject);
    procedure btnDescriptionFontClick(Sender: TObject);
    procedure btnHeaderFontClick(Sender: TObject);
    procedure chbUseHintAsHeaderClick(Sender: TObject);
    procedure chbUseStandardFooterClick(Sender: TObject);
    procedure edtWidthChange(Sender: TObject);
    procedure edtWidthExit(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure pbHeaderClick(Sender: TObject);
    procedure pbHeaderContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure pbHeaderPaint(Sender: TObject);
    procedure pbStandardFooterClick(Sender: TObject);
    procedure pbStandardFooterPaint(Sender: TObject);
    procedure pnlPreviewResize(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Right1Click(Sender: TObject);
    procedure FixedWidth1Click(Sender: TObject);
    procedure btnDescriptionRichTextEditClick(Sender: TObject);
    procedure chbPlainTextClick(Sender: TObject);
    procedure btnShowOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FEditBand: TdxCustomScreenTipBand;
    FLockUpdate: Integer;
    FPopupTrackPaintBox: TPaintBox;
    FRefreshing: Boolean;
    FViewInfo: TdxCustomHintViewInfo;
    procedure ApplyRichText(Sender: TObject);
    procedure DrawBandImage(APaintBox: TPaintBox);
    function GetBand(AIndex: Integer): TdxCustomScreenTipBand;
    function GetRepository: TdxScreenTipRepository;
    function GetScreenTip: TdxScreenTip;
    procedure OpenBandImage(APaintBox: TPaintBox);
    procedure RefreshHint;
    procedure SetBandText(AEdit: TCustomEdit);
    procedure UpdateFontInfo(AFontInfoViewer: TLabel);
    procedure UpdateHeaderFont;
    procedure UpdateDescriptionFont;
    procedure UpdateFooterFont;
    procedure UpdateScreenTipInfo;
    procedure UpdateRepositoryInfo;
  protected
    procedure DataChanged;
    procedure DoSelectionIfNoItemsSelected; override;
    procedure InitFormEditor; override;
  public
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    // IEditHandler
    function EditAction(Action: DesignIntf.TEditAction): Boolean; override;
    function GetEditState: TEditState; override;

    procedure DoItemsModified; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
    property Repository: TdxScreenTipRepository read GetRepository;
    property ScreenTip: TdxScreenTip read GetScreenTip;
  end;

  TdxScreenTipCollectionProperty = class(TcxComponentCollectionProperty)
  public
    function GetEditorClass: TcxComponentCollectionEditorClass; override;
  end;

implementation

uses
  Math, cxLookAndFeels, cxLookAndFeelPainters, cxGeometry,
  cxControls, cxLibraryReg, dxRichTextEditor;

const
  BandTypeAsString: array [0..3] of string = (
    'Header', 'Description', 'Footer', 'Standard footer');
  ShowOptionsButtonText: array [Boolean] of string = (
    'Show options', 'Hide options');

{$R *.dfm}

{ TfrmScreenTipRepositoryEditor }

procedure TfrmScreenTipRepositoryEditor.BeginUpdate;
begin
  Inc(FLockUpdate);
end;

procedure TfrmScreenTipRepositoryEditor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScreenTipRepositoryEditor.btnDescriptionFontClick(
  Sender: TObject);
begin
  FontDialog1.Font := Repository.DescriptionFont;
  if FontDialog1.Execute then
  begin
    Repository.DescriptionFont := FontDialog1.Font;
    UpdateDescriptionFont;
    DataChanged;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.btnFooterFontClick(Sender: TObject);
begin
  FontDialog1.Font := Repository.FooterFont;
  if FontDialog1.Execute then
  begin
    Repository.FooterFont := FontDialog1.Font;
    UpdateFooterFont;
    DataChanged;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.btnHeaderFontClick(Sender: TObject);
begin
  FontDialog1.Font := Repository.HeaderFont;
  if FontDialog1.Execute then
  begin
    Repository.HeaderFont := FontDialog1.Font;
    UpdateHeaderFont;
    DataChanged;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.btnShowOptionsClick(Sender: TObject);
begin
  pnlOptions.Visible := not pnlOptions.Visible;
  btnShowOptions.Caption := ShowOptionsButtonText[pnlOptions.Visible];
end;

procedure TfrmScreenTipRepositoryEditor.CancelUpdate;
begin
  Dec(FLockUpdate);
end;

procedure TfrmScreenTipRepositoryEditor.chbHeaderFontClick(Sender: TObject);
var
  AFontType: TdxScreenTipBandType;
begin
  AFontType := TdxScreenTipBandType((Sender as TCheckBox).Tag);
  if (Sender as TCheckBox).Checked then
    Repository.AssignedFonts := Repository.AssignedFonts + [AFontType]
  else
    Repository.AssignedFonts := Repository.AssignedFonts - [AFontType];
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.chbShowDescriptionClick(
  Sender: TObject);
begin
  Repository.ShowDescription := chbShowDescription.Checked;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.chbUseHintAsHeaderClick(
  Sender: TObject);
begin
  if ScreenTip = nil then Exit;
  ScreenTip.UseHintAsHeader := chbUseHintAsHeader.Checked;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.chbUseStandardFooterClick(
  Sender: TObject);
begin
  if ScreenTip = nil then Exit;
  ScreenTip.UseStandardFooter := chbUseStandardFooter.Checked;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.chbPlainTextClick(Sender: TObject);
var
  ABand: TdxCustomScreenTipBand;
begin
  if (ScreenTip = nil) and ((Sender as TComponent).Tag < 3) then Exit;
  ABand := GetBand((Sender as TComponent).Tag);
  ABand.PlainText := (Sender as TCheckBox).Checked;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.Clear1Click(Sender: TObject);
begin
  GetBand(FPopupTrackPaintBox.Tag).Glyph := nil;
  FPopupTrackPaintBox.Invalidate;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.FixedWidth1Click(Sender: TObject);
begin
  GetBand(FPopupTrackPaintBox.Tag).GlyphFixedWidth := not GetBand(FPopupTrackPaintBox.Tag).GlyphFixedWidth;
  FPopupTrackPaintBox.Invalidate;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.FormCreate(Sender: TObject);
begin
  inherited;
  pnlPreviewAndOptions.Align := alClient;
end;

procedure TfrmScreenTipRepositoryEditor.EndUpdate;
begin
  CancelUpdate;
  RefreshHint;
end;

function TfrmScreenTipRepositoryEditor.EditAction(Action: DesignIntf.TEditAction): Boolean;
var
  AEdit: TCustomEdit;
begin
  if ActiveControl is TCustomEdit then
  begin
    AEdit := TCustomEdit(ActiveControl);
    case Action of
      eaCut:
        AEdit.CutToClipboard;
      eaCopy:
        AEdit.CopyToClipboard;
      eaPaste:
        AEdit.PasteFromClipboard;
      eaSelectAll:
        AEdit.SelectAll;
    end;
    Result := True;
  end
  else
    Result := inherited EditAction(Action);
end;

function TfrmScreenTipRepositoryEditor.GetEditState: TEditState;
begin
  Result := [esCanCut..esCanPaste, esCanSelectAll];
end;

procedure TfrmScreenTipRepositoryEditor.DoItemsModified;
begin
  inherited DoItemsModified;
  UpdateRepositoryInfo;
  UpdateScreenTipInfo;
  RefreshHint;
end;

function TfrmScreenTipRepositoryEditor.IsShortCut(var Message: TWMKey): Boolean;
begin
  if ListView1.Focused then
    Result := inherited IsShortCut(Message)
  else
    Result := False;
end;

procedure TfrmScreenTipRepositoryEditor.edtHeaderChange(Sender: TObject);
begin
  if ScreenTip <> nil then
    SetBandText(Sender as TCustomEdit);
end;

procedure TfrmScreenTipRepositoryEditor.edtStandardFooterChange(
  Sender: TObject);
begin
  SetBandText(Sender as TCustomEdit);
end;

procedure TfrmScreenTipRepositoryEditor.edtWidthChange(Sender: TObject);
var
  ANewValue: Integer;
begin
  if ScreenTip = nil then Exit;
  if TryStrToInt(edtWidth.Text, ANewValue) and InRange(ANewValue, 0, UpDown1.Max) and
    (ScreenTip.Width <> ANewValue) then
  begin
    ScreenTip.Width := ANewValue;
    DataChanged;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.edtWidthExit(Sender: TObject);
begin
  edtWidth.Text := IntToStr(UpDown1.Position);
end;

procedure TfrmScreenTipRepositoryEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
begin
  inherited SelectionsChanged(ASelection);
  UpdateRepositoryInfo;
  UpdateScreenTipInfo;
  RefreshHint;
end;

procedure TfrmScreenTipRepositoryEditor.DataChanged;
begin
  if FLockUpdate > 0 then Exit;
  Designer.Modified;
end;

procedure TfrmScreenTipRepositoryEditor.DoSelectionIfNoItemsSelected;
begin
  Designer.SelectComponent(Component);
end;

procedure TfrmScreenTipRepositoryEditor.InitFormEditor;

  procedure SelectFirstItemIfAvailable;
  begin
    if ListView1.Items.Count > 0 then
      ListView1.Items[0].Selected := True;
    SetSelection;
  end;

begin
  inherited InitFormEditor;
  OpenPictureDialog1.Filter := BuildcxBitmapPropertyFilter;
  SelectFirstItemIfAvailable;
end;

procedure TfrmScreenTipRepositoryEditor.ApplyRichText(Sender: TObject);
begin
  FEditBand.Text := (Sender as TfrmRichTextEditor).Text;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.DrawBandImage(APaintBox: TPaintBox);
begin
  cxDrawImage(APaintBox.Canvas.Handle, APaintBox.ClientRect,
    APaintBox.ClientRect, GetBand(APaintBox.Tag).Glyph, nil, -1, idmNormal);
end;

function TfrmScreenTipRepositoryEditor.GetBand(AIndex: Integer): TdxCustomScreenTipBand;
begin
  case AIndex of
    0: Result := ScreenTip.Header;
    1: Result := ScreenTip.Description;
    2: Result := ScreenTip.Footer;
  else {StandardFooter - 3}
    Result := Repository.StandardFooter;
  end;
end;

function TfrmScreenTipRepositoryEditor.GetRepository: TdxScreenTipRepository;
begin
  Result := Component as TdxScreenTipRepository;
end;

function TfrmScreenTipRepositoryEditor.GetScreenTip: TdxScreenTip;
begin
  if ListView1.Selected <> nil then
    Result := Repository.Items[ListView1.Selected.Index]
  else
    Result := nil;
end;

procedure TfrmScreenTipRepositoryEditor.Open1Click(Sender: TObject);
begin
  OpenBandImage(FPopupTrackPaintBox);
end;

procedure TfrmScreenTipRepositoryEditor.OpenBandImage(APaintBox: TPaintBox);
var
  ABand: TdxCustomScreenTipBand;
  APicture: TPicture;
begin
  if OpenPictureDialog1.Execute then
  begin
    ABand := GetBand(APaintBox.Tag);
    APicture := TPicture.Create;
    try
      APicture.LoadFromFile(OpenPictureDialog1.FileName);
      if APicture.Graphic <> nil then
        ABand.Glyph.Assign(APicture.Graphic)
      else
        ABand.Glyph := nil;
    finally
      APicture.Free;
    end;
    APaintBox.Invalidate;
    DataChanged;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.PaintBox1Paint(Sender: TObject);
begin
  if (ScreenTip <> nil) and (FViewInfo <> nil) then
    FViewInfo.Paint(PaintBox1.Canvas);
end;

procedure TfrmScreenTipRepositoryEditor.pbHeaderClick(Sender: TObject);
begin
  if ScreenTip <> nil then
    OpenBandImage(Sender as TPaintBox);
end;

procedure TfrmScreenTipRepositoryEditor.pbHeaderContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  FPopupTrackPaintBox := Sender as TPaintBox;
end;

procedure TfrmScreenTipRepositoryEditor.pbHeaderPaint(Sender: TObject);
begin
  if ScreenTip <> nil then
    DrawBandImage(Sender as TPaintBox);
end;

procedure TfrmScreenTipRepositoryEditor.pbStandardFooterClick(Sender: TObject);
begin
  OpenBandImage(pbStandardFooter);
end;

procedure TfrmScreenTipRepositoryEditor.pbStandardFooterPaint(Sender: TObject);
begin
  DrawBandImage(pbStandardFooter);
end;

procedure TfrmScreenTipRepositoryEditor.pnlPreviewResize(Sender: TObject);
begin
  RefreshHint;
end;

procedure TfrmScreenTipRepositoryEditor.PopupMenu3Popup(Sender: TObject);
begin
  if GetBand(FPopupTrackPaintBox.Tag).TextAlign = stbtaLeft then
    Right1.Checked := True
  else
    Left1.Checked := True;
  Clear1.Enabled := GetBand(FPopupTrackPaintBox.Tag).HasGlyph;
  FixedWidth1.Checked := GetBand(FPopupTrackPaintBox.Tag).GlyphFixedWidth;
end;

procedure TfrmScreenTipRepositoryEditor.RefreshHint;

  function CreateViewInfo(const AHint: string; AScreenTip: TdxScreenTip;
    APainter: TcxCustomLookAndFeelPainter): TdxCustomHintViewInfo;
  var
    AHintText: string;
  begin
    AHintText := AHint;
    if (AScreenTip <> nil) and AScreenTip.Collection.Repository.ShowDescription then
      Result := TdxScreenTipViewInfo.Create(AScreenTip, APainter, AHintText)
    else
    begin
      if (AScreenTip <> nil) and not AScreenTip.UseHintAsHeader and (AScreenTip.Header.Text <> '') then
        AHintText := AScreenTip.Header.Text;
      Result := TdxScreenTipLikeHintViewInfo.Create(APainter, AHintText, '', cxNullPoint);
    end;
  end;

begin
  if FRefreshing then Exit;
  FRefreshing := True;
  try
    FreeAndNil(FViewInfo);
    if (FLockUpdate <= 0) and (ScreenTip <> nil) then
    begin
      FViewInfo := CreateViewInfo('<Hint provided by hinted control>', ScreenTip,
        RootLookAndFeel.Painter);
      FViewInfo.Calculate(PaintBox1.Canvas);
      PaintBox1.BoundsRect := cxRectCenter(gbPreview.ClientRect,
        cxRectSize(FViewInfo.BoundsRect));
      if PaintBox1.Height > gbPreview.ClientHeight - 20 then
        PaintBox1.Top := 10;
      if PaintBox1.Width > gbPreview.ClientWidth then
        PaintBox1.Left := 0;
    end;
    PaintBox1.Repaint;
  finally
    FRefreshing := False;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.Right1Click(Sender: TObject);
begin
  GetBand(FPopupTrackPaintBox.Tag).TextAlign :=
    TdxScreenTipBandTextAlign((Sender as TComponent).Tag);
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.SetBandText(AEdit: TCustomEdit);
begin
  GetBand(AEdit.Tag).Text := AEdit.Text;
  DataChanged;
end;

procedure TfrmScreenTipRepositoryEditor.btnDescriptionRichTextEditClick(Sender: TObject);
var
  ARichTextEditor: TfrmRichTextEditor;
begin
  if (ScreenTip = nil) and ((Sender as TComponent).Tag < 3) then Exit;
  FEditBand := GetBand((Sender as TComponent).Tag);
  ARichTextEditor := TfrmRichTextEditor.Create(Self);
  try
    ARichTextEditor.OnApply := ApplyRichText;
    ARichTextEditor.edtRich.PlainText := FEditBand.PlainText;
    ARichTextEditor.edtRich.Font := FEditBand.Font;
    ARichTextEditor.Text := FEditBand.Text;
    ARichTextEditor.Caption := BandTypeAsString[(Sender as TComponent).Tag] +
      IfThen(FEditBand.PlainText, ' - Plain', ' - Rich') + ' text mode';
    ARichTextEditor.Position := poOwnerFormCenter;
    if ARichTextEditor.Execute and ARichTextEditor.Modified then
    begin
      FEditBand.Text := ARichTextEditor.Text;
      DataChanged;
    end;
  finally
    FEditBand := nil;
    ARichTextEditor.Release;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.UpdateFontInfo(AFontInfoViewer: TLabel);
var
  AFont: TFont;
begin
  case TdxScreenTipBandType(AFontInfoViewer.Tag) of
    stbHeader: AFont := Repository.HeaderFont;
    stbDescription: AFont := Repository.DescriptionFont;
  else {stbFooter}
    AFont := Repository.FooterFont;
  end;
  AFontInfoViewer.Font.Color := AFont.Color;
  AFontInfoViewer.Font.Style := AFont.Style;
  AFontInfoViewer.Caption := Format('(%s, %d)', [AFont.Name, AFont.Size]);
  chbHeaderFont.Checked := stbHeader in Repository.AssignedFonts;
  chbDescriptionFont.Checked := stbDescription in Repository.AssignedFonts;
  chbFooterFont.Checked := stbFooter in Repository.AssignedFonts;
end;

procedure TfrmScreenTipRepositoryEditor.UpdateHeaderFont;
begin
  UpdateFontInfo(lblHeaderFontInfo);
end;

procedure TfrmScreenTipRepositoryEditor.UpdateDescriptionFont;
begin
  UpdateFontInfo(lblDescriptionFontInfo);
end;

procedure TfrmScreenTipRepositoryEditor.UpdateFooterFont;
begin
  UpdateFontInfo(lblFooterFontInfo);
end;

procedure TfrmScreenTipRepositoryEditor.UpdateScreenTipInfo;
begin
  BeginUpdate;
  try
    if ScreenTip <> nil then
    begin
      gbScreenTipInfo.Enabled := True;
      edtHeader.Text := ScreenTip.Header.Text;
      edtDescription.Text := ScreenTip.Description.Text;
      edtFooter.Text := ScreenTip.Footer.Text;
      chbUseHintAsHeader.Checked := ScreenTip.UseHintAsHeader;
      chbUseStandardFooter.Checked := ScreenTip.UseStandardFooter;
      UpDown1.Position := ScreenTip.Width;
      chbHeaderPlain.Checked := ScreenTip.Header.PlainText;
      chbDescriptionPlain.Checked := ScreenTip.Description.PlainText;
      chbFooterPlain.Checked := ScreenTip.Footer.PlainText;
      edtHeader.Enabled := ScreenTip.Header.PlainText;
      edtDescription.Enabled := ScreenTip.Description.PlainText;
      edtFooter.Enabled := ScreenTip.Footer.PlainText;
//      btnHeaderRichTextEdit.Enabled := not ScreenTip.Header.PlainText;
//      btnDescriptionRichTextEdit.Enabled := not ScreenTip.Description.PlainText;
//      btnFooterRichTextEdit.Enabled := not ScreenTip.Footer.PlainText;
    end
    else
    begin
      edtHeader.Clear;
      edtDescription.Clear;
      edtFooter.Clear;
      chbUseHintAsHeader.Checked := False;
      chbUseStandardFooter.Checked := False;
      chbHeaderPlain.Checked := False;
      chbDescriptionPlain.Checked := False;
      chbFooterPlain.Checked := False;
      UpDown1.Position := 0;
      gbScreenTipInfo.Enabled := False;
    end;
    pbFooter.Invalidate;
    pbHeader.Invalidate;
    pbDescription.Invalidate;
  finally
    CancelUpdate;
  end;
end;

procedure TfrmScreenTipRepositoryEditor.UpdateRepositoryInfo;
begin
  BeginUpdate;
  try
    edtStandardFooter.Text := Repository.StandardFooter.Text;
    pbStandardFooter.Invalidate;
    edtStandardFooter.Enabled := Repository.StandardFooter.PlainText;
   // btnStdFooterRichTextEdit.Enabled := not Repository.StandardFooter.PlainText;
    chbPlainText.Checked := Repository.StandardFooter.PlainText;
    chbShowDescription.Checked := Repository.ShowDescription;
    UpdateHeaderFont;
    UpdateDescriptionFont;
    UpdateFooterFont;
  finally
    CancelUpdate;
  end;
end;

{ TdxScreenTipCollectionProperty }

function TdxScreenTipCollectionProperty.GetEditorClass: TcxComponentCollectionEditorClass;
begin
  Result := TfrmScreenTipRepositoryEditor;
end;

end.
