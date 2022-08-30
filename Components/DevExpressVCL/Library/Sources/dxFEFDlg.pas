{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxFEFDlg;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtDlgs, Registry,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Forms, Menus, Consts, ImgList, IniFiles,
  dxCore, dxPSForm, dxBkgnd, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer,
  cxEdit, cxGroupBox, cxLabel, cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox,
  cxPC, dxExtCtrls, dxPSReportRenderCanvas, cxLookAndFeels, dxLayoutContainer, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxImageList, cxGeometry;

type
  TdxFillAs = (faNone, faTexture, faPattern, faPicture);

  { TdxFEFDialog }

  TdxFEFDialog = class(TCustomdxPSForm)
    btnApply: TcxButton;
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnInvert: TcxButton;
    btnOK: TcxButton;
    btnOtherTexture: TcxButton;
    btnPreview: TcxButton;
    btnSelectPicture: TcxButton;
    cbxBackColor: TcxColorComboBox;
    cbxForeColor: TcxColorComboBox;
    cbxPaintMode: TcxComboBox;
    dgPattern: TDrawGrid;
    dgTexture: TDrawGrid;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    ilMenu: TcxImageList;
    lblApply: TdxLayoutItem;
    lblBackground: TdxLayoutItem;
    lblForeground: TdxLayoutItem;
    lblHelp: TdxLayoutItem;
    lblPaintMode: TdxLayoutItem;
    lblSample: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgTabbed: TdxLayoutGroup;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miDelete: TMenuItem;
    miLoad: TMenuItem;
    miPaste: TMenuItem;
    miPreview: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    pbxPreview: TPaintBox;
    pmPicture: TPopupMenu;
    pnlPatternName: TdxLayoutLabeledItem;
    pnlPicture: TdxLayoutItem;
    pnlTextureName: TdxLayoutLabeledItem;
    tshPattern: TdxLayoutGroup;
    tshPicture: TdxLayoutGroup;
    tshTexture: TdxLayoutGroup;

    procedure btnApplyClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnOtherTextureClick(Sender: TObject);
    procedure cbxColorChange(Sender: TObject);
    procedure cbxPaintModeChange(Sender: TObject);
    procedure dgPatternClick(Sender: TObject);
    procedure dgPatternDblClick(Sender: TObject);
    procedure dgPatternDrawCell(Sender: TObject; Col, Row: Integer; Rect: TRect; State: TGridDrawState);
    procedure dgPatternMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure dgTextureClick(Sender: TObject);
    procedure dgTextureDblClick(Sender: TObject);
    procedure dgTextureDrawCell(Sender: TObject; Col, Row: Integer; Rect: TRect; State: TGridDrawState);
    procedure dgTextureMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lblShowCombo(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure pbxPicturePaint(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure PicturePreviewClick(Sender: TObject);
    procedure pmPicturePopup(Sender: TObject);
    procedure SelectPictureClick(Sender: TObject);
  private
    FActivePage: Integer;
    FApplied: Boolean;
    FBackground: TdxBackground;
    FbmpPattern: TBitmap;
    FbmpCurrentPattern: TBitmap;
    FbmpTexture: TBitmap;
    FControlsUpdating: Boolean;
    FFirstApplied: Boolean;
    FInitialDir: string;
    FModified: Boolean;
    FOriginalBackground: TdxBackground;
    FOtherPicture: TGraphic;
    FOtherPictureName: string;
    FOtherTexture: TBitmap;
    FOtherTextureName: string;
    FPatternNames: TStringList;
    FPatternWasSelected: Boolean;
    FPicture: TGraphic;
    FPictureExists: Boolean;
    FPreviewWhat: TdxFillAs;
    FTextureWasSelected: Boolean;
    FTextureNames: TStringList;
    FOnApply: TNotifyEvent;

    function GetBackColor: TColor;
    function GetForeColor: TColor;
    function GetOtherPicture(AGraphicClass: TGraphicClass): TGraphic;
    function GetOtherTexture: TBitmap;
    function GetPaintMode: TdxPicturePaintMode;
    function GetPicture: TGraphic;
    function GetSelectWhat: TdxFillAs;
    procedure SetBackColor(Value: TColor);
    procedure SetBackground(Value: TdxBackground);
    procedure SetForeColor(Value: TColor);
    procedure SetOtherTexture(Value: TBitmap);
    procedure SetPaintMode(Value: TdxPicturePaintMode);
    procedure SetPicture(Value: TGraphic);
    procedure SetSelectWhat(Value: TdxFillAs);

    procedure AssignPicture(AImage: TGraphic);
    procedure CheckModified;
    function CopyPattern(I, J: Integer): TBitmap;
    function CopyTexture(I, J: Integer): TBitmap;
    procedure DrawSelectedFrame(ADrawGrid: TDrawGrid; Rect: TRect);
    procedure DoApply;
    procedure DoInvertColors;
    procedure DoPictureChanged;
    procedure FreeAndNilResources;
    function InternalLoadImage(var AImage: TGraphic; const AFileName: string): Boolean;
    procedure LoadImage(var AImage: TGraphic; AWhat: Integer);
    procedure LoadResources;
    procedure MapPatternColors;
    procedure PaintPreview(ACanvas: TCanvas; const R: TRect);
    procedure PrepareGrids;
    procedure SetupDialog;
    procedure SetupDialogPatterns;
    procedure SetupDialogTextures;
    procedure StartSetting;
    procedure UpdateControlsState;

    procedure WMCancelMode(var Msg: TWMCancelMode); message WM_CANCELMODE;

    property BackColor: TColor read GetBackColor write SetBackColor;
    property ForeColor: TColor read GetForeColor write SetForeColor;
    property OtherTexture: TBitmap read GetOtherTexture write SetOtherTexture;
    property PaintMode: TdxPicturePaintMode read GetPaintMode write SetPaintMode;
    property Picture: TGraphic read GetPicture write SetPicture;
    property SelectWhat: TdxFillAs read GetSelectWhat write SetSelectWhat;
  protected
    sbxPicture: TdxPSImageScrollBox;

    procedure CreateInternalControls; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure ScaleFactorChanged(M: Integer; D: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;

    property Background: TdxBackground read FBackground write SetBackground;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

function dxFEFDialog(ABackground: TdxBackground): Boolean;
function StandardGetGraphicClassProc(const AFileName: string): TGraphicClass;

type
  TdxGetGraphicClassProc = function(const AFileName: string): TGraphicClass;

var
  GetGraphicClassProc: TdxGetGraphicClassProc = StandardGetGraphicClassProc;

implementation

{$R *.DFM}

uses
 {$IFDEF USEJPEGIMAGE}
  Jpeg,
 {$ENDIF}
  ClipBrd, dxPSGlbl, dxPcPrVw, dxPSPopupMan, dxPSImgs, dxPSRes, dxPSUtl;

type
  TdxBackgroundAccess = class(TdxBackground);

const
  PatternCount: TPoint = (X: 8; Y: 6);
  PatternSize: TPoint = (X: 8; Y: 8);
  TextureCount: TPoint = (X: 4; Y: 6);
  TextureSize: TPoint = (X: 64; Y: 64);

  sdxInitialDir = 'InitialDir';                //Don't Localize
  sdxActivePage = 'ActivePage';                //Don't Localize

function dxCreateMappedBmp(ASource: TBitmap; const OldColors, NewColors: array of TColor): TBitmap;
var
  I, J, K: Integer;
begin
  Result := TBitmap.Create;
  try
    Result.Width := ASource.Width;
    Result.Height := ASource.Height;
    for I := 0 to ASource.Width - 1 do
      for J := 0 to ASource.Height - 1 do
        for K := Low(OldColors) to High(OldColors) do
          if ASource.Canvas.Pixels[I, J] <> OldColors[K] then
            Result.Canvas.Pixels[I, J] := ASource.Canvas.Pixels[I, J]
          else
          begin
            Result.Canvas.Pixels[I, J] := NewColors[K];
            Break;
          end;
  except
    Result.Free;
    raise;
  end;
end;

function dxFEFDialog(ABackground: TdxBackground): Boolean;
var
  Dialog: TdxFEFDialog;
  B: TBitmap;
begin
  Result := False;
  if ABackground = nil then Exit;

  Dialog := TdxFEFDialog.Create(nil);
  try
    Dialog.Background := ABackground;
    Dialog.FOriginalBackground := ABackground;
    Dialog.OnApply := TdxBackgroundAccess(ABackground).OnApply;
    Result := Dialog.Execute or not Dialog.FFirstApplied; {at least one time button "Apply" was pressed}
    if Dialog.ModalResult <> mrOK then Exit;
    if Result then
    begin
      ABackground.BkColor := Dialog.BackColor;
      ABackground.Brush.Color := Dialog.ForeColor;
      ABackground.Picture := Dialog.Picture;
      case Dialog.SelectWhat of
        faTexture:
          ABackground.Mode := bmBrushBitmap;
        faPattern:
          begin
            B := TBitmap(ABackground.Picture);
            B.Width := PatternSize.X;
            B.Height := PatternSize.Y;
            B.Canvas.Draw(-Dialog.dgPattern.Col * PatternSize.X,
              -Dialog.dgPattern.Row * PatternSize.Y, Dialog.FbmpCurrentPattern);
            ABackground.Mode := bmBrushBitmap;
          end;
        faPicture:
          begin
            ABackground.Mode := bmPicture;
            ABackground.PictureMode := Dialog.PaintMode;
          end;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

function StandardGetGraphicClassProc(const AFileName: string): TGraphicClass;
var
  Extention: string;
begin
  Result := nil;
  Extention := ExtractFileExt(AFileName);
  if CompareText(Extention, '.' + cxGraphicExtension(TBitmap)) = 0 then
    Result := TBitmap
  else
   {$IFDEF USEJPEGIMAGE}
    if CompareText(Extention, '.' + cxGraphicExtension(TJpegImage)) = 0 then
      Result := TJpegImage
    else
   {$ENDIF}
      if CompareText(Extention, '.' + cxGraphicExtension(TMetafile)) = 0 then
        Result := TMetafile
      else
        if CompareText(Extention, '.wmf') = 0 then
          Result := TMetafile;
end;

{ TdxFEFDialog }

constructor TdxFEFDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateInternalControls;
  HelpContext := dxhcFEFDlg;
  FBackground := TdxBackground.Create;
  FPicture := TBitmap.Create;
  FPatternNames := TStringList.Create;
  FTextureNames := TStringList.Create;

  FFirstApplied := True;
  LoadResources;
  FPreviewWhat := faNone;
  FPatternWasSelected := False;
  FTextureWasSelected := False;
  FPictureExists := False;

  dxLoadImageListFromResources(ilMenu, IDIL_DXPSDESIGNWINDOWMENU);
end;

destructor TdxFEFDialog.Destroy;
begin
  FreeAndNil(FPatternNames);
  FreeAndNil(FTextureNames);
  FreeAndNil(FBackground);
  FreeAndNil(FPicture);
  FreeAndNil(FOtherTexture);
  FreeAndNil(FOtherPicture);
  FreeAndNilResources;
  inherited Destroy;
end;

procedure TdxFEFDialog.CreateInternalControls;
begin
  sbxPicture := TdxPSImageScrollBox.Create(Self);
  sbxPicture.SetBounds(0, 0, 0, 0);
  pnlPicture.Control := sbxPicture;
  sbxPicture.PopupMenu := pmPicture;
end;

procedure TdxFEFDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS;
end;

procedure TdxFEFDialog.Loaded;
begin
  inherited Loaded;
  PrepareGrids;
end;

procedure TdxFEFDialog.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  PrepareGrids;
end;

procedure TdxFEFDialog.WMCancelMode(var Msg: TWMCancelMode);
begin
  inherited;
  dgPattern.Invalidate;
  dgTexture.Invalidate;
end;

function TdxFEFDialog.Execute: Boolean;
begin
  StartSetting;
  Result := (ShowModal = mrOk) and FModified;
end;

procedure TdxFEFDialog.SetBackground(Value: TdxBackground);
begin
  FBackground.Assign(Value);
  SetupDialog;
  UpdateControlsState;
end;

procedure TdxFEFDialog.SetupDialog;
begin
  FControlsUpdating := True;
  try
    cbxPaintMode.ItemIndex := Integer(Background.PictureMode);
    ForeColor := FBackground.Brush.Color;
    BackColor := FBackground.BkColor;
    if (BackColor = ForeColor) and (ForeColor = clWhite) then
      ForeColor := clBlack;
    MapPatternColors;
    if Assigned(FBackground.Picture) then
    begin
      case FBackground.Mode of
        bmBrushBitmap:
          begin
            SetupDialogPatterns;
            SetupDialogTextures;
          end;

        bmPicture:
          begin
            FPicture.Assign(TBitmap(FBackground.Picture));
            GetOtherPicture(TGraphicClass(FBackground.Picture.ClassType)).Assign(FBackground.Picture);
            FPictureExists := True;
            FPreviewWhat := faPicture;
            DoPictureChanged;
          end;
      end;
    end;
  finally
    FControlsUpdating := False;
  end;
  pbxPreview.Invalidate;
end;

procedure TdxFEFDialog.SetupDialogPatterns;
var
  ABmp: TBitmap;
  I, J: Integer;
begin
  for I := 0 to PatternCount.X - 1 do
    for J := 0 to PatternCount.Y - 1 do
    begin
      ABmp := CopyPattern(I, J);
      try
        if dxAreGraphicsEqual(ABmp, FBackground.Picture) then
        begin
          FPatternWasSelected := True;
          dgPattern.Col := I;
          dgPattern.Row := J;
          FPicture.Assign(TBitmap(FBackground.Picture));
          FPreviewWhat := faPattern;
          dgPatternClick(dgPattern);
        end;
      finally
        ABmp.Free;
      end;
    end;
end;

procedure TdxFEFDialog.SetupDialogTextures;
var
  I, J: Integer;
  ABmp: TBitmap;
begin
  for I := 0 to TextureCount.X - 1 do
    for J := 0 to TextureCount.Y - 1 do
    begin
      ABmp := CopyTexture(I, J);
      try
        if dxAreBitmapsEqual(ABmp, TBitmap(FBackground.Picture)) then
        begin
          FTextureWasSelected := True;
          dgTexture.Col := I;
          dgTexture.Row := J;
          if (dgTexture.Row > 2) then
            dgTexture.TopRow := dgTexture.Row - 2;
          SetPicture(TBitmap(FBackground.Picture));
          FPreviewWhat := faTexture;
          dgTextureClick(dgTexture);
        end;
      finally
        ABmp.Free;
      end;
    end;
end;

procedure TdxFEFDialog.UpdateControlsState;
begin
  lblApply.Visible := Assigned(OnApply);
  btnApply.Enabled := FModified and Assigned(FPicture) and not (FPicture.Empty or FApplied);
  btnInvert.Enabled := BackColor <> ForeColor;
  btnPreview.Enabled := (FOtherPicture <> nil) and
    ((FOtherPicture.Width > sbxPicture.ClientWidth) or
    (FOtherPicture.Height > sbxPicture.ClientHeight));
  lblPaintMode.Enabled := FOtherPicture <> nil;
end;

procedure TdxFEFDialog.CheckModified;
begin
  if not FModified then FModified := True;
  FApplied := False;
  UpdateControlsState;
end;

procedure TdxFEFDialog.StartSetting;
begin
  FModified := False;
  FControlsUpdating := True;
  try
    if FPreviewWhat = faNone then
      lgTabbed.ItemIndex := FActivePage
    else
      SetSelectWhat(FPreviewWhat);

    CheckDialogFormHelpContext(Self, lblHelp);
  finally
    UpdateControlsState;
    FControlsUpdating := False;
  end;
end;

procedure TdxFEFDialog.LoadResources;
var
  Index: Integer;
begin
  FbmpPattern := TBitmap.Create;
  FbmpPattern.Width := PatternCount.X * PatternSize.X;
  FbmpPattern.Height := PatternCount.Y * PatternSize.Y;
  dxLoadBitmapFromResource(FbmpPattern, IDB_DXPSBKPATTERNS);

  FbmpTexture := TBitmap.Create;
  FbmpTexture.Width := TextureCount.X * TextureSize.X;
  FbmpTexture.Height := TextureCount.Y * TextureSize.Y;
  dxLoadBitmapFromResource(FbmpTexture, IDB_DXPSBKTEXTURES);

  miLoad.Caption := cxGetResourceString(@sdxMenuLoad);
  miPreview.Caption := cxGetResourceString(@sdxMenuPreview);
  miCut.Caption := cxGetResourceString(@sdxMenuEditCut);
  miCopy.Caption := cxGetResourceString(@sdxMenuEditCopy);
  miPaste.Caption := cxGetResourceString(@sdxMenuEditPaste);
  miDelete.Caption := cxGetResourceString(@sdxMenuEditDelete);

  tshTexture.Caption := cxGetResourceString(@sdxTexture);
  tshPattern.Caption := cxGetResourceString(@sdxPattern);
  tshPicture.Caption := cxGetResourceString(@sdxPicture);

  btnOtherTexture.Caption := cxGetResourceString(@sdxBtnOtherTexture);
  lblForeground.Caption := cxGetResourceString(@sdxForeground);
  lblBackground.Caption := cxGetResourceString(@sdxBackground);
  btnInvert.Caption := cxGetResourceString(@sdxBtnInvertColors);
  sbxPicture.HintText := cxGetResourceString(@sdxThereIsNoPictureToDisplay);
  btnPreview.Caption := cxGetResourceString(@sdxBtnPreview);
  btnSelectPicture.Caption := cxGetResourceString(@sdxBtnSelectPicture);
  lblPaintMode.Caption := cxGetResourceString(@sdxPaintMode);
  lblSample.Caption := cxGetResourceString(@sdxSample);
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnApply.Caption := cxGetResourceString(@sdxBtnApply);
  Caption := cxGetResourceString(@sdxFEFCaption);

  Index := cbxPaintMode.ItemIndex;
  with cbxPaintMode.Properties do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      Items.Add(cxGetResourceString(@sdxPaintModeCenter));
      Items.Add(cxGetResourceString(@sdxPaintModeStretch));
      Items.Add(cxGetResourceString(@sdxPaintModeTile));
      Items.Add(cxGetResourceString(@sdxPaintModeProportional));
    finally
      Items.EndUpdate;
    end;
  end;
  cbxPaintMode.ItemIndex := Index;

  with FPatternNames do
  begin
    BeginUpdate;
    try
      Clear;
      Add(cxGetResourceString(@sdxPatternGray5));
      Add(cxGetResourceString(@sdxPatternGray10));
      Add(cxGetResourceString(@sdxPatternGray20));
      Add(cxGetResourceString(@sdxPatternGray25));
      Add(cxGetResourceString(@sdxPatternGray30));
      Add(cxGetResourceString(@sdxPatternGray40));
      Add(cxGetResourceString(@sdxPatternGray50));
      Add(cxGetResourceString(@sdxPatternGray60));
      Add(cxGetResourceString(@sdxPatternGray70));
      Add(cxGetResourceString(@sdxPatternGray75));
      Add(cxGetResourceString(@sdxPatternGray80));
      Add(cxGetResourceString(@sdxPatternGray90));
      Add(cxGetResourceString(@sdxPatternLightDownwardDiagonal));
      Add(cxGetResourceString(@sdxPatternLightUpwardDiagonal));
      Add(cxGetResourceString(@sdxPatternDarkDownwardDiagonal));
      Add(cxGetResourceString(@sdxPatternDarkUpwardDiagonal));
      Add(cxGetResourceString(@sdxPatternWideDownwardDiagonal));
      Add(cxGetResourceString(@sdxPatternWideUpwardDiagonal));
      Add(cxGetResourceString(@sdxPatternLightVertical));
      Add(cxGetResourceString(@sdxPatternLightHorizontal));
      Add(cxGetResourceString(@sdxPatternNarrowVertical));
      Add(cxGetResourceString(@sdxPatternNarrowHorizontal));
      Add(cxGetResourceString(@sdxPatternDarkVertical));
      Add(cxGetResourceString(@sdxPatternDarkHorizontal));
      Add(cxGetResourceString(@sdxPatternDashedDownward));
      Add(cxGetResourceString(@sdxPatternDashedUpward));
      Add(cxGetResourceString(@sdxPatternDashedHorizontal));
      Add(cxGetResourceString(@sdxPatternDashedVertical));
      Add(cxGetResourceString(@sdxPatternSmallConfetti));
      Add(cxGetResourceString(@sdxPatternLargeConfetti));
      Add(cxGetResourceString(@sdxPatternZigZag));
      Add(cxGetResourceString(@sdxPatternWave));
      Add(cxGetResourceString(@sdxPatternDiagonalBrick));
      Add(cxGetResourceString(@sdxPatternHorizantalBrick));
      Add(cxGetResourceString(@sdxPatternWeave));
      Add(cxGetResourceString(@sdxPatternPlaid));
      Add(cxGetResourceString(@sdxPatternDivot));
      Add(cxGetResourceString(@sdxPatternDottedGrid));
      Add(cxGetResourceString(@sdxPatternDottedDiamond));
      Add(cxGetResourceString(@sdxPatternShingle));
      Add(cxGetResourceString(@sdxPatternTrellis));
      Add(cxGetResourceString(@sdxPatternSphere));
      Add(cxGetResourceString(@sdxPatternSmallGrid));
      Add(cxGetResourceString(@sdxPatternLargeGrid));
      Add(cxGetResourceString(@sdxPatternSmallCheckedBoard));
      Add(cxGetResourceString(@sdxPatternLargeCheckedBoard));
      Add(cxGetResourceString(@sdxPatternOutlinedDiamond));
      Add(cxGetResourceString(@sdxPatternSolidDiamond));
    finally
      EndUpdate;
    end;
  end;

  with FTextureNames do
  begin
    BeginUpdate;
    try
      Clear;
      Add(cxGetResourceString(@sdxTextureNewSprint));
      Add(cxGetResourceString(@sdxTextureGreenMarble));
      Add(cxGetResourceString(@sdxTextureBlueTissuePaper));
      Add(cxGetResourceString(@sdxTexturePapyrus));
      Add(cxGetResourceString(@sdxTextureWaterDroplets));
      Add(cxGetResourceString(@sdxTextureCork));
      Add(cxGetResourceString(@sdxTextureRecycledPaper));
      Add(cxGetResourceString(@sdxTextureWhiteMarble));
      Add(cxGetResourceString(@sdxTexturePinkMarble));
      Add(cxGetResourceString(@sdxTextureCanvas));
      Add(cxGetResourceString(@sdxTexturePaperBag));
      Add(cxGetResourceString(@sdxTextureWalnut));
      Add(cxGetResourceString(@sdxTextureParchment));
      Add(cxGetResourceString(@sdxTextureBrownMarble));
      Add(cxGetResourceString(@sdxTexturePurpleMesh));
      Add(cxGetResourceString(@sdxTextureDenim));
      Add(cxGetResourceString(@sdxTextureFishFossil));
      Add(cxGetResourceString(@sdxTextureOak));
      Add(cxGetResourceString(@sdxTextureStationary));
      Add(cxGetResourceString(@sdxTextureGranite));
      Add(cxGetResourceString(@sdxTextureBouquet));
      Add(cxGetResourceString(@sdxTextureWonenMat));
      Add(cxGetResourceString(@sdxTextureSand));
      Add(cxGetResourceString(@sdxTextureMediumWood));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxFEFDialog.FreeAndNilResources;
begin
  FreeAndNil(FbmpPattern);
  FreeAndNil(FbmpCurrentPattern);
  FreeAndNil(FbmpTexture);
end;

procedure TdxFEFDialog.dgTextureDrawCell(Sender: TObject;
  Col, Row: Integer; Rect: TRect; State: TGridDrawState);
var
  DrawGrid: TDrawGrid;
  R: TRect;
  DC: HDC;
  BPP: Integer;
  DoHalftone: Boolean;
  Pt: TPoint;
  ABitmap: Windows.TBitmap;
  BltMode: Integer;
begin
  DrawGrid := TDrawGrid(Sender);
  DC := DrawGrid.Canvas.Handle;
  R := Rect;
  if not ((gdSelected in State) and FTextureWasSelected) then
    if (FOtherTexture = nil) or ((Row < DrawGrid.RowCount - 1) or (Col = 0)) then
    begin
      DrawEdge(DC, R, EDGE_SUNKEN, BF_RECT);
      InflateRect(R, -2, -2);
    end
    else
  else
    InflateRect(R, -2, -2);

  BltMode := GetStretchBltMode(DC);
  if (FOtherTexture = nil) or (Row < DrawGrid.RowCount - 1) then
  begin
    BPP := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
    cxGetBitmapData(FbmpTexture.Handle, ABitmap);
    DoHalftone := (BPP <= 8) and (BPP < (ABitmap.bmBitsPixel * ABitmap.bmPlanes));
    if DoHalftone then
    begin
      GetBrushOrgEx(DC, Pt);
      SetStretchBltMode(DC, HALFTONE);
      SetBrushOrgEx(DC, Pt.x, Pt.y, @Pt);
    end
    else
      if not FbmpTexture.Monochrome then
        SetStretchBltMode(DC, STRETCH_DELETESCANS);
  end;

  if FOtherTexture = nil then
    Windows.StretchBlt(DC, R.Left, R.Top, R.Right - R.Left,
      R.Bottom - R.Top, FbmpTexture.Canvas.Handle, Col * TextureSize.X,
      Row * TextureSize.Y, TextureSize.X, TextureSize.Y, SRCCOPY)
  else
    if Row < DrawGrid.RowCount - 1 then
      Windows.StretchBlt(DC, R.Left, R.Top, R.Right - R.Left,
        R.Bottom - R.Top, FbmpTexture.Canvas.Handle, Col * TextureSize.X,
        Row * TextureSize.Y, TextureSize.X, TextureSize.Y, SRCCOPY)
     else
       if Col = 0 then
         DrawGrid.Canvas.StretchDraw(R, FOtherTexture)
       else
         Windows.FillRect(DC, Rect, hBrush(COLOR_BTNFACE + 1));

  SetStretchBltMode(DC, BltMode);

  if gdSelected in State then
    if (FTextureWasSelected and (FOtherTexture = nil)) or
      ((FOtherTexture <> nil) and ((Row < DrawGrid.RowCount - 1) or (DrawGrid.Col = 0))) then
      DrawSelectedFrame(DrawGrid, Rect);
end;

procedure TdxFEFDialog.dgPatternDrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
var
  APrevBrush: TBrush;
  ABrushBitmap: TBitmap;
  Pt: TPoint;
  DC: hDC;
begin
  DC := TDrawGrid(Sender).Canvas.Handle;
  if not ((gdSelected in State) and FPatternWasSelected) then
    DrawEdge(DC, Rect, EDGE_SUNKEN, BF_RECT);
  InflateRect(Rect, -2, -2);

  APrevBrush := TBrush.Create;
  try
    APrevBrush.Assign(TDrawGrid(Sender).Canvas.Brush);
    ABrushBitmap := CopyPattern(Col, Row);
    try
      SetBrushOrgEx(DC, Rect.Left, Rect.Top, @Pt);
      with TDrawGrid(Sender).Canvas do
      begin
        Brush.Bitmap := ABrushBitmap;
        FillRect(Rect);
        Brush.Bitmap := nil;
        Brush := APrevBrush;
      end;
      SetBrushOrgEx(DC, Pt.X, Pt.Y, nil);
    finally
      ABrushBitmap.Free;
    end;
  finally
    APrevBrush.Free;
  end;

  if (gdSelected in State) and FPatternWasSelected then
  begin
    InflateRect(Rect, 2, 2);
    DrawSelectedFrame(TDrawGrid(Sender), Rect);
  end;
end;

procedure TdxFEFDialog.DrawSelectedFrame(ADrawGrid: TDrawGrid; Rect: TRect);
var
  DC: HDC;
  PrevColor: TColor;
  PrevMode: Integer;
  Points: array of TPoint;
begin
  with ADrawGrid do
  begin
    DC := Canvas.Handle;
    if (ActiveControl = ADrawGrid) and Focused then
    begin
      //InflateRect(Rect, 2, 2);
      PrevMode := SetBkMode(DC, TRANSPARENT);
      PrevColor := Canvas.Pen.Color;
      Canvas.Pen.Color := clWindowText;
      Canvas.Pen.Style := psDot;
      with Rect do
      begin
        SetLength(Points, 5);
        Points[0] := TopLeft;
        Points[1] := Point(Right - 1, Top);
        Points[2] := Point(Right - 1, Bottom - 1);
        Points[3] := Point(Left, Bottom - 1);
        Points[4] := TopLeft;
        Canvas.Polyline(Points);
      end;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := PrevColor;
      SetBkMode(DC, PrevMode);
      InflateRect(Rect, -1, -1);
    end;
    DrawEdge(DC, Rect, BDR_SUNKENOUTER, BF_RECT or BF_MONO);
    InflateRect(Rect, -1, -1);
    FrameRect(DC, Rect, GetSysColorBrush(COLOR_BTNHIGHLIGHT));
  end;
end;

procedure TdxFEFDialog.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  FInitialDir := AIniFile.ReadString(ASectionName, sdxInitialDir, FInitialDir);
  FActivePage := AIniFile.ReadInteger(ASectionName, sdxActivePage, FActivePage);
end;

procedure TdxFEFDialog.SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  AIniFile.WriteString(ASectionName, sdxInitialDir, FInitialDir);
  AIniFile.WriteInteger(ASectionName, sdxActivePage, lgTabbed.ItemIndex);
end;

procedure TdxFEFDialog.SelectPictureClick(Sender: TObject);
var
  Graphic: TGraphic;
begin
  Graphic := nil;
  try
    LoadImage(Graphic, 1);
    if Assigned(Graphic) then
      AssignPicture(Graphic);
  finally
    FreeAndNil(Graphic);
  end;
end;

procedure TdxFEFDialog.AssignPicture(AImage: TGraphic);
begin
  if AImage is TMetafile then
  begin
    FPicture.Free;
    FPicture := TBitmap.Create;
    FPicture.Width := AImage.Width;
    FPicture.Height := AImage.Height;
    TBitmap(FPicture).Palette := AImage.Palette;
    TBitmap(FPicture).Canvas.Draw(0, 0, AImage);
  end
  else
    SetPicture(AImage);

  GetOtherPicture(TGraphicClass(FPicture.ClassType)).Assign(FPicture);
  DoPictureChanged;
  CheckModified;
  FPreviewWhat := faPicture;
  pbxPreview.Invalidate;
  FPictureExists := True;
end;

procedure TdxFEFDialog.DoPictureChanged;
begin
  if sbxPicture.Picture.Graphic <> FOtherPicture then
  begin
    if FOtherPicture = nil then
      sbxPicture.Picture.Graphic := nil
    else
      sbxPicture.Picture.Graphic := FOtherPicture;

    sbxPicture.Invalidate;
    pbxPreview.Invalidate;
  end;
end;

procedure TdxFEFDialog.pbxPicturePaint(Sender: TObject);
begin
  if FOtherPicture <> nil then
    TPaintBox(Sender).Canvas.Draw(0, 0, FOtherPicture);
end;

procedure TdxFEFDialog.btnOtherTextureClick(Sender: TObject);
var
  APicture: TGraphic;
begin
  APicture := nil;
  try
    LoadImage(APicture, 0);
    if APicture <> nil then
    begin
      if APicture is TMetafile then
      begin
        FPicture.Free;
        FPicture := TBitmap.Create;
        FPicture.Width := TextureSize.X;
        FPicture.Height := TextureSize.Y;
        TBitmap(FPicture).Palette := APicture.Palette;
        TBitmap(FPicture).Canvas.Draw(0, 0, APicture);
      end
      else
        SetPicture(APicture);

      if FOtherTexture = nil then
        dgTexture.RowCount := dgTexture.RowCount + 1;
      OtherTexture.Assign(FPicture);
      dgTexture.Col := 0;
      dgTexture.Row := dgTexture.RowCount - 1;
      if not FTextureWasSelected then
        FTextureWasSelected := True;
      dgTexture.Invalidate;
      CheckModified;
      FPreviewWhat := faTexture;
      pbxPreview.Refresh;
    end;
  finally
    APicture.Free;
  end;
end;

procedure TdxFEFDialog.LoadImage(var AImage: TGraphic; AWhat: Integer);

  function UnregisterIcon: Boolean;
  begin
    Result := Pos('ico', cxGraphicFilter(TGraphic)) > 0;
    if Result then
      TPicture.UnregisterGraphicClass(TIcon);
  end;

  procedure RegisterIcon(ARegistered: Boolean);
  begin
    if ARegistered then
      TPicture.RegisterFileFormat('ico', SVIcons, TIcon);
  end;

var
  ARegistered: Boolean;
  Dialog: TOpenPictureDialog;
  FileName: string;
  B: TBitmap;
begin
  ARegistered := UnregisterIcon;
  try
    Dialog := TOpenPictureDialog.Create(nil);
    try
      Dialog.InitialDir := FInitialDir;
      Dialog.Filter := cxGraphicFilter(TGraphic);
      if Dialog.Execute then
      begin
        if InternalLoadImage(AImage, Dialog.Filename) then
        begin
          FInitialDir := ExtractFileDir(Dialog.Filename);
          FileName := {Dialog.FileName;//}ChangeFileExt(ExtractFileName(Dialog.Filename), '');

          case AWhat of
            0: // textures
              begin
                FOtherTextureName := FileName;
                pnlTextureName.Caption := FileName;//ChangeFileExt(ExtractFileName(Filename), '');
                if AImage is TMetafile then
                //
                else
                  if AImage is TBitmap then
                  begin
                    AImage.Width := TextureSize.X;
                    AImage.Height := TextureSize.Y;
                  end
                  else
                  begin
                    B := TBitmap.Create;
                    try
                      B.Assign(AImage);
                      B.Width := TextureSize.X;
                      B.Height := TextureSize.Y;
                      AImage.Assign(B);
                    finally
                      B.Free;
                    end;
                  end;
              end;

            1: // pictures
              begin
                FOtherPictureName := FileName;
                pnlPicture.Caption := FileName;//ChangeFileExt(ExtractFileName(FileName), '');
              end;
          end;
          CheckModified;
        end;
      end;
    finally
      Dialog.Free;
    end;
  finally
    RegisterIcon(ARegistered);
  end;
end;

function TdxFEFDialog.InternalLoadImage(var AImage: TGraphic; const AFileName: string): Boolean;
var
  Picture: TPicture;
begin
  AImage := nil;
  Picture := TPicture.Create;
  try
    try
      Picture.LoadFromFile(AFileName);
      AImage := dxPSUtl.CreateGraphic(TGraphicClass(Picture.Graphic.ClassType));
      AImage.Assign(Picture.Graphic);
      Result := True;
    except
      FreeAndNil(AImage);
      Result := False;
    end;
  finally
    Picture.Free;
  end;
end;

function TdxFEFDialog.GetPicture: TGraphic;
begin
  Result := FPicture;
end;

function TdxFEFDialog.GetPaintMode: TdxPicturePaintMode;
begin
  if cbxPaintMode.ItemIndex > -1 then
    Result := TdxPicturePaintMode(cbxPaintMode.ItemIndex)
  else
    Result := ppmCenter;
end;

procedure TdxFEFDialog.SetPicture(Value: TGraphic);
begin
  if FPicture = nil then
    FPicture := TBitmap.Create;
  FPicture.Assign(Value);
end;

procedure TdxFEFDialog.SetPaintMode(Value: TdxPicturePaintMode);
begin
  if PaintMode <> Value then
    cbxPaintMode.ItemIndex := Integer(Value);
end;

procedure TdxFEFDialog.PaintPreview(ACanvas: TCanvas; const R: TRect);

  function GetImageSize(AImage: TGraphic): TSize;
  begin
    Result := dxGetImageSize(AImage, ScaleFactor);
  end;

  procedure TileDraw(AGraphic: TGraphic);
  var
    AImageSize: TSize;
    I, J: Integer;
  begin
    AImageSize := GetImageSize(AGraphic);
    for I := 0 to cxRectWidth(R) div AImageSize.cx do
      for J := 0 to cxRectHeight(R) div AImageSize.cy do
        ACanvas.StretchDraw(Bounds(I * AImageSize.cx, J * AImageSize.cy, AImageSize.cx, AImageSize.cy), AGraphic);
  end;

  procedure DrawImage(AImage: TGraphic; AMode: TdxPicturePaintMode);
  begin
    case AMode of
      ppmStretch:
        ACanvas.StretchDraw(R, AImage);
      ppmTile:
        TileDraw(AImage);
      ppmCenter:
        ACanvas.StretchDraw(cxRectCenter(R, GetImageSize(AImage)), AImage);
      ppmProportional:
        ACanvas.StretchDraw(cxRectProportionalStretch(R, GetImageSize(AImage)), AImage);
    end;
  end;

var
  ABrushBitmap: TBitmap;
  APrevBrush: TBrush;
begin
  case FPreviewWhat of
    faTexture:
      if FTextureWasSelected then
        TileDraw(Picture);

    faPicture:
      if FOtherPicture <> nil then
        DrawImage(FOtherPicture, PaintMode);

    faPattern:
      if FPatternWasSelected then
      begin
        APrevBrush := TBrush.Create;
        try
          APrevBrush.Assign(pbxPreview.Canvas.Brush);
          ABrushBitmap := CopyPattern(dgPattern.Col, dgPattern.Row);
          try
            ACanvas.Brush.Bitmap := ABrushBitmap;
            ACanvas.FillRect(R);
            ACanvas.Brush.Bitmap := nil;
            ACanvas.Brush := APrevBrush;
          finally
            ABrushBitmap.Free;
          end;
        finally
          APrevBrush.Free;
        end;
      end;

  end;
end;

procedure TdxFEFDialog.PrepareGrids;
begin
  dgTexture.DefaultColWidth := (dgTexture.Width - GetSystemMetrics(SM_CXHSCROLL) - ScaleFactor.Apply(1)) div
    dgTexture.ColCount - ScaleFactor.Apply(1);
  dgTexture.DefaultRowHeight := dgTexture.DefaultColWidth;

  dgPattern.DefaultColWidth := (dgPattern.Width - ScaleFactor.Apply(1)) div dgPattern.ColCount - ScaleFactor.Apply(1);
  dgPattern.DefaultRowHeight := (dgPattern.Height - ScaleFactor.Apply(1)) div dgPattern.RowCount - ScaleFactor.Apply(1);
end;

procedure TdxFEFDialog.dgTextureClick(Sender: TObject);
var
  DrawGrid: TDrawGrid;
begin
  DrawGrid := TDrawGrid(Sender);
  FreeAndNil(FPicture);

  if FOtherTexture = nil then
    FPicture := CopyTexture(DrawGrid.Col, DrawGrid.Row)
  else
    if (DrawGrid.Row = DrawGrid.RowCount - 1) and (DrawGrid.Col > 0) then
      Exit
    else
      if DrawGrid.Row < DrawGrid.RowCount - 1 then
        FPicture := CopyTexture(DrawGrid.Col, DrawGrid.Row)
      else
        if DrawGrid.Col = 0 then
          SetPicture(OtherTexture);

  if not FTextureWasSelected then
  begin
    FTextureWasSelected := True;
    DrawGrid.Invalidate;
  end;
  FPreviewWhat := faTexture;
  if Assigned(FOtherTexture) and (DrawGrid.Col = 0) and (DrawGrid.Row = DrawGrid.RowCount - 1) then
    pnlTextureName.Caption := FOtherTextureName
  else
    pnlTextureName.Caption := FTextureNames[DrawGrid.Col * TextureCount.Y + DrawGrid.Row];
  CheckModified;
  pbxPreview.Invalidate;
end;

procedure TdxFEFDialog.dgPatternClick(Sender: TObject);
var
  DrawGrid: TDrawGrid;
begin
  DrawGrid := TDrawGrid(Sender);
  if not FPatternWasSelected then
  begin
    FPatternWasSelected := True;
    DrawGrid.Invalidate;
  end;
  FreeAndNil(FPicture);

  FPicture := CopyPattern(DrawGrid.Col, DrawGrid.Row);
  FPreviewWhat := faPattern;
  pbxPreview.Refresh;
  pnlPatternName.Caption := FPatternNames[DrawGrid.Col * TextureCount.Y + DrawGrid.Row];
  CheckModified;
end;

procedure TdxFEFDialog.pbxPreviewPaint(Sender: TObject);
begin
  PaintPreview(pbxPreview.Canvas, cxRectInflate(pbxPreview.ClientRect, ScaleFactor.Apply(2)));
end;

procedure TdxFEFDialog.MapPatternColors;
begin
  FreeAndNil(FbmpCurrentPattern);
  FbmpCurrentPattern := dxCreateMappedBmp(FbmpPattern, [clWhite, clBlack], [BackColor, ForeColor]);
end;

procedure TdxFEFDialog.cbxColorChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FPreviewWhat := faPattern;
  MapPatternColors;
  pbxPreview.Invalidate;
  dgPattern.Invalidate;
  if (dgPattern.Col > -1) and (dgPattern.Row > -1) then
    CheckModified;
end;

function TdxFEFDialog.CopyTexture(I, J: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  with Result do
  try
    Width := TextureSize.X;
    Height := TextureSize.Y;
    Canvas.Draw(-I * TextureSize.X, -J * TextureSize.Y, FbmpTexture);
  except
    Free;
    raise;
  end;
end;

function TdxFEFDialog.CopyPattern(I, J: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  with Result do
  try
    Width := PatternSize.X;
    Height := PatternSize.Y;
    Canvas.Draw(-I * PatternSize.X, -J * PatternSize.Y, FbmpCurrentPattern);
  except
    Result.Free;
    raise;
  end;
end;

procedure TdxFEFDialog.PageControl1Change(Sender: TObject);
begin
  FApplied := False;
  FPreviewWhat := TdxFillAs(lgTabbed.ItemIndex + 1);
  FreeAndNil(FPicture);

  case FPreviewWhat of
    faPattern:
      if FPatternWasSelected then
        FPicture := CopyPattern(dgPattern.Col, dgPattern.Row);

    faPicture:
      if FOtherPicture <> nil then
        SetPicture(FOtherPicture);

    faTexture:
      if FTextureWasSelected then
      begin
        if FOtherTexture = nil then
          FPicture := CopyTexture(dgTexture.Col, dgTexture.Row)
        else
          if dgTexture.Row < dgTexture.RowCount - 1 then
            FPicture := CopyTexture(dgTexture.Col, dgTexture.Row)
          else
            if dgTexture.Col = 0 then
              SetPicture(OtherTexture);
      end;
  end;
  pbxPreview.Invalidate;
  UpdateControlsState;
end;

procedure TdxFEFDialog.PicturePreviewClick(Sender: TObject);
begin
  dxShowPicturePreview(Picture);
end;

procedure TdxFEFDialog.cbxPaintModeChange(Sender: TObject);
begin
  pbxPreview.Invalidate;
  if not FControlsUpdating then
    CheckModified;
end;

function TdxFEFDialog.GetSelectWhat: TdxFillAs;
begin
  if FTextureWasSelected and (lgTabbed.ItemIndex = 0) then
    Result := faTexture
  else
    if FPatternWasSelected and (lgTabbed.ItemIndex = 1) then
      Result := faPattern
    else
      if FPictureExists and (lgTabbed.ItemIndex = 2) then
        Result := faPicture
      else
        Result := faNone;
end;

procedure TdxFEFDialog.SetSelectWhat(Value: TdxFillAs);
begin
  case Value of
    faTexture:
      begin
        FTextureWasSelected := True;
        lgTabbed.ItemIndex := tshTexture.Index;
      end;

    faPattern:
      begin
        FPatternWasSelected := True;
        lgTabbed.ItemIndex := tshPattern.Index;
      end;

    faPicture:
      begin
        FPictureExists := True;
        lgTabbed.ItemIndex := tshPicture.Index;
      end;
  end;
end;

function TdxFEFDialog.GetOtherTexture: TBitmap;
begin
  if FOtherTexture = nil then
    FOtherTexture := TBitmap.Create;
  Result := FOtherTexture;
end;

procedure TdxFEFDialog.SetOtherTexture(Value: TBitmap);
begin
  if Assigned(Value) then
    GetOtherTexture.Assign(Value)
  else
    FreeAndNil(FOtherTexture);
end;

function TdxFEFDialog.GetOtherPicture(AGraphicClass: TGraphicClass): TGraphic;
begin
  FreeAndNil(FOtherPicture);
  if AGraphicClass <> nil then
    FOtherPicture := dxPSUtl.CreateGraphic(AGraphicClass);
  DoPictureChanged;
  Result := FOtherPicture;
end;

function TdxFEFDialog.GetForeColor: TColor;
begin
  Result := cbxForeColor.ColorValue;
end;

procedure TdxFEFDialog.SetForeColor(Value: TColor);
begin
  cbxForeColor.ColorValue := Value;
  cbxColorChange(cbxForeColor); {???}
end;

function TdxFEFDialog.GetBackColor: TColor;
begin
  Result := cbxBackColor.ColorValue;
end;

procedure TdxFEFDialog.SetBackColor(Value: TColor);
begin
  cbxBackColor.ColorValue := Value;
  cbxColorChange(cbxBackColor); {???}
end;

procedure TdxFEFDialog.lblShowCombo(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

procedure TdxFEFDialog.DoApply;
var
  B: TBitmap;
begin
  if Assigned(FOnApply) then
  begin
    Background.BeginUpdate;
    try
      Background.BkColor := BackColor;
      Background.Brush.Color := ForeColor;
      Background.Picture := Picture;
      case SelectWhat of
        faTexture:
          begin
            Background.Picture := Picture;
            Background.Mode := bmBrushBitmap;
          end;

        faPattern:
          begin
            B := TBitmap(Background.Picture);
            B.Width := PatternSize.X;
            B.Height := PatternSize.Y;
            B.Canvas.Draw(-dgPattern.Col * PatternSize.X,
              -dgPattern.Row * PatternSize.Y, FbmpCurrentPattern);
            Background.Mode := bmBrushBitmap;
          end;

        faPicture:
          begin
            Background.Mode := bmPicture;
            Background.PictureMode := PaintMode;
          end;
      end;
      if FOriginalBackground <> nil then
        FOriginalBackground.Assign(Background);
      FOnApply(Background);
    finally
      Background.EndUpdate;
    end;
  end;
end;

procedure TdxFEFDialog.btnApplyClick(Sender: TObject);
begin
  DoApply;
  FApplied := True;
  if FFirstApplied then
  begin
    btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
    FFirstApplied := False;
  end;
  UpdateControlsState;
end;

procedure TdxFEFDialog.dgTextureDblClick(Sender: TObject);
begin
  if FPicture <> nil then
    btnOK.Click;
end;

procedure TdxFEFDialog.dgPatternDblClick(Sender: TObject);
begin
  if FPicture <> nil then
    btnOK.Click;
end;

procedure TdxFEFDialog.dgTextureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  ATextureLastCol: Longint = -2;
  ATextureLastRow: Longint = -2;
var
  Col, Row: Longint;
  S: string;
begin
  TDrawGrid(Sender).MouseToCell(X, Y, Col, Row);

  if (Col <> ATextureLastCol) or (Row <> ATextureLastRow) then
  begin
    Application.CancelHint;
    S := '';
    if (Col > -1) and (Row > -1) then
      if FOtherTexture = nil then
        S := FTextureNames[Col * TextureCount.Y + Row]
      else
        if (FOtherTexture <> nil) and (Col = 0) and (Row = TDrawGrid(Sender).RowCount - 1) then
          S := FOtherTextureName
        else
          if Row < TDrawGrid(Sender).RowCount - 1 then
            S := FTextureNames[Col * TextureCount.Y + Row];

    TDrawGrid(Sender).Hint := S;
  end;

  ATextureLastCol := Col;
  ATextureLastRow := Row;
end;

procedure TdxFEFDialog.dgPatternMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  APatternLastCol: Longint = -2;
  APatternLastRow: Longint = -2;
var
  Col, Row: Longint;
begin
  TDrawGrid(Sender).MouseToCell(X, Y, Col, Row);
  if (Col <> APatternLastCol) or (Row <> APatternLastRow) then
  begin
    Application.CancelHint;
    if (Col > -1) and (Row > -1) then
      TDrawGrid(Sender).Hint := FPatternNames[Col * PatternCount.Y + Row];
  end;

  APatternLastCol := Col;
  APatternLastRow := Row;
end;

procedure TdxFEFDialog.btnInvertClick(Sender: TObject);
begin
  DoInvertColors;
  with dgPattern do
  begin
    if (Col > -1) and (Row > -1) then
      CheckModified;
  end;
end;

procedure TdxFEFDialog.DoInvertColors;
var
  ASaveColor: TColor;
begin
  dgPattern.Perform(WM_SETREDRAW, WPARAM(False), 0);
  btnInvert.Perform(WM_SETREDRAW, WPARAM(False), 0);
  ASaveColor := ForeColor;
  ForeColor := BackColor;
  BackColor := ASaveColor;
  dgPattern.Perform(WM_SETREDRAW, WPARAM(True), 0);
  dgPattern.Invalidate;
  btnInvert.Perform(WM_SETREDRAW, WPARAM(True), 0);
  btnInvert.Invalidate;
end;

procedure TdxFEFDialog.pmPicturePopup(Sender: TObject);
begin
  miPreview.Enabled := btnPreview.Enabled;
  miCut.Enabled := FOtherPicture <> nil;
  miCopy.Enabled := FOtherPicture <> nil;
  miPaste.Enabled := ClipBoard.HasFormat(CF_PICTURE);
  miDelete.Enabled := FOtherPicture <> nil;
end;

procedure TdxFEFDialog.miCopyClick(Sender: TObject);
begin
  ClipBoard.Assign(FOtherPicture);
end;

procedure TdxFEFDialog.miPasteClick(Sender: TObject);
var
  P: TPicture;
begin
  P := TPicture.Create;
  try
    P.Assign(Clipboard);
    if (P.Graphic <> nil) and not P.Graphic.Empty then
      AssignPicture(P.Graphic);
  finally
    P.Free;
  end;
end;

procedure TdxFEFDialog.miDeleteClick(Sender: TObject);
begin
  GetOtherPicture(nil);
  sbxPicture.Invalidate;
end;

procedure TdxFEFDialog.miCutClick(Sender: TObject);
begin
  miCopy.Click;
  miDelete.Click;
end;

end.
