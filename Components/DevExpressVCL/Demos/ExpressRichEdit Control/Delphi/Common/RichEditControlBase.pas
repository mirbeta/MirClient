unit RichEditControlBase;

interface

{$I cxVer.Inc}
{$R RichEditControlDemoAppGlyphs.res}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF EXPRESSSKINS}
  dxSkinsdxRibbonPainter,
{$ENDIF}
  Forms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxRibbonCustomizationForm, dxRibbonSkins, dxCore, dxCoreClasses, dxGDIPlusAPI,
  dxGDIPlusClasses, dxRichEdit.Types, dxRichEdit.Options, dxRichEdit.Control,
  dxBarBuiltInMenu, dxBar, dxBarApplicationMenu, dxRibbon, dxScreenTip, ImgList,
  Controls, Classes, ActnList, dxRibbonGallery, dxSkinChooserGallery, dxRibbonForm,
  dxStatusBar, dxRibbonStatusBar, dxRichEdit.Platform.Win.Control, cxClasses,
  RibbonRichEditDemoOptions;

type
  TfrmRichEditControlBase = class(TdxRibbonForm)
    Ribbon: TdxRibbon;
    bmBarManager: TdxBarManager;
    acActions: TActionList;
    bbApplicationButton: TdxBarLargeButton;
    bbQATVisible: TdxBarLargeButton;
    bbRibbonForm: TdxBarLargeButton;
    scgiLookAndFeel: TdxSkinChooserGalleryItem;
    acQATAboveRibbon: TAction;
    acQATBelowRibbon: TAction;
    stBarScreenTips: TdxScreenTipRepository;
    cxLookAndFeelController: TcxLookAndFeelController;
    ApplicationMenu: TdxBarApplicationMenu;
    rsbStatusBar: TdxRibbonStatusBar;
    ilSmallImages: TcxImageList;
    ilLargeImages: TcxImageList;
    ilLargeColorSchemesGlyphs: TcxImageList;
    ilSmallColorSchemesGlyphs: TcxImageList;
    procedure bbRibbonFormClick(Sender: TObject);
    procedure acQATApplicationButtonExecute(Sender: TObject);
    procedure bbQATVisibleClick(Sender: TObject);
    procedure acQATAboveAndBelowRibbonExecute(Sender: TObject);
    procedure acQATAboveAndBelowRibbonUpdate(Sender: TObject);
    procedure scgiLookAndFeelPopulate(Sender: TObject);
    procedure scgiLookAndFeelSelected(Sender: TObject; const ASkinName: string);
    procedure bbOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetRibbonDemoStyle: TRibbonDemoStyle;
    procedure InitializeLookAndFeel;
  protected
    procedure SetColorScheme(const AName: string); virtual;
    procedure SetRibbonDemoStyle(const AStyle: TRibbonDemoStyle); virtual;

    property RibbonDemoStyle: TRibbonDemoStyle read GetRibbonDemoStyle write SetRibbonDemoStyle;
  end;

var
  frmRichEditControlBase: TfrmRichEditControlBase;

implementation

uses
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsCore, dxSkinsForm, dxSkinsdxBarPainter, dxSkinscxPCPainter,
{$ENDIF}
  SysUtils, Windows;

{$R *.dfm}

procedure TfrmRichEditControlBase.acQATAboveAndBelowRibbonUpdate(Sender: TObject);
begin
  acQATAboveRibbon.Checked := Ribbon.QuickAccessToolbar.Position = qtpAboveRibbon;
  acQATBelowRibbon.Checked := Ribbon.QuickAccessToolbar.Position = qtpBelowRibbon;
end;

procedure TfrmRichEditControlBase.acQATAboveAndBelowRibbonExecute(
  Sender: TObject);
begin
  if TAction(Sender).Tag <> 0 then
    Ribbon.QuickAccessToolbar.Position := qtpBelowRibbon
  else
    Ribbon.QuickAccessToolbar.Position := qtpAboveRibbon;
end;

procedure TfrmRichEditControlBase.acQATApplicationButtonExecute(Sender: TObject);
begin
  Ribbon.ApplicationButton.Visible := bbApplicationButton.Down;
end;

procedure TfrmRichEditControlBase.bbOptionsClick(Sender: TObject);
var
  AColorSchemeAccent: TdxRibbonColorSchemeAccent;
  AColorSchemeName: string;
  AScreenTipOptions: TScreenTipOptions;
  AStyle: TRibbonDemoStyle;
begin
  AStyle := RibbonDemoStyle;
  AColorSchemeName := Ribbon.ColorSchemeName;
  AColorSchemeAccent := Ribbon.ColorSchemeAccent;
  AScreenTipOptions.ShowScreenTips := bmBarManager.ShowHint;
  AScreenTipOptions.ShowDescripitons := stBarScreenTips.ShowDescription;
  if ExecuteRibbonDemoOptions(AColorSchemeName, AScreenTipOptions, AStyle, AColorSchemeAccent) then
  begin
    RibbonDemoStyle := AStyle;
    Ribbon.ColorSchemeAccent := AColorSchemeAccent;
    bmBarManager.ShowHint := AScreenTipOptions.ShowScreenTips;
    stBarScreenTips.ShowDescription := AScreenTipOptions.ShowDescripitons;
    SetColorScheme(AColorSchemeName);
  end;
end;

procedure TfrmRichEditControlBase.bbQATVisibleClick(Sender: TObject);
begin
  Ribbon.QuickAccessToolbar.Visible := bbQATVisible.Down;
end;

procedure TfrmRichEditControlBase.bbRibbonFormClick(Sender: TObject);
begin
  Ribbon.SupportNonClientDrawing := bbRibbonForm.Down;
end;

procedure TfrmRichEditControlBase.FormCreate(Sender: TObject);
begin
{$IFDEF EXPRESSSKINS}
  TdxSkinController.Create(Self);
{$ENDIF}
  InitializeLookAndFeel;
  bbRibbonForm.Down := Ribbon.SupportNonClientDrawing;
  bbApplicationButton.Down := Ribbon.ApplicationButton.Visible;
  bbQATVisible.Down := Ribbon.QuickAccessToolbar.Visible;
end;

function TfrmRichEditControlBase.GetRibbonDemoStyle: TRibbonDemoStyle;
begin
  case Ribbon.Style of
    rs2007:
      Result := rdsOffice2007;
    rs2013:
      Result := rdsOffice2013;
    rs2016:
      Result := rdsOffice2016;
    rs2016Tablet:
      Result := rdsOffice2016Tablet;
    rs2019:
      Result := rdsOffice2019;
  else
    if Ribbon.EnableTabAero then
      Result := rdsOffice2010
    else
      Result := rdsScenic;
  end;
end;

procedure TfrmRichEditControlBase.InitializeLookAndFeel;
begin
  cxLookAndFeelController.NativeStyle := False;
  cxLookAndFeelController.SkinName := 'DevExpressStyle';
  scgiLookAndFeel.SelectedSkinName := RootLookAndFeel.Painter.LookAndFeelName;
  RibbonDemoStyle := rdsOffice2019;
end;

procedure TfrmRichEditControlBase.scgiLookAndFeelPopulate(Sender: TObject);

  function GetGlyphIndex(const ASkinName: string; out AIndex: Integer): Boolean;
  const
    NameMap: array[0..7] of string = (
      'Blue', 'Black', 'Silver', 'DarkGray', 'LightGray', 'White', 'MediumGray', 'Colorful'
    );
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(NameMap) - 1 do
      if SameText(ASkinName, NameMap[I]) then
      begin
        Result := True;
        AIndex := I;
        Break;
      end;
  end;

  procedure SetGlyph(ABitmap: TdxSmartImage; AImageList: TcxImageList; AGlyphIndex: Integer);
  var
    ABitmap32: TcxBitmap32;
  begin
    if Ribbon.Style in [rs2016, rs2016Tablet, rs2019] then
      case AGlyphIndex of
        3: //DarkGray
          AGlyphIndex := 1;
        4: //LightGray
          AGlyphIndex := 2;
        6: //DarkGray
          AGlyphIndex := 3;
        7: //Colorful
          AGlyphIndex := 0;
      end;
    ABitmap32 := TcxBitmap32.CreateSize(AImageList.Width, AImageList.Height, True);
    try
      AImageList.Draw(ABitmap32.Canvas, 0, 0, AGlyphIndex);
      ABitmap.Assign(ABitmap32);
    finally
      ABitmap32.Free;
    end;
  end;

const
  DisplayNameMap: array[0..7] of string = (
    'Blue', 'Black', 'Silver', 'Dark Gray', 'Light Gray', 'White', 'Medium Gray', 'Colorful'
  );
  RibbonColorSchemesGroupName = 'Ribbon Color Schemes';
var
  AIndex: Integer;
  ASkin: TdxCustomRibbonSkin;
{$IFDEF EXPRESSSKINS}
  ASkinDetails: TdxSkinDetails;
{$ENDIF}
  ASkinItem: TdxSkinChooserGalleryGroupItem;
  I: Integer;
begin
  for I := 0 to dxRibbonSkinsManager.SkinCount - 1 do
  begin
    ASkin := dxRibbonSkinsManager.Skins[I];
    if ASkin.Style = Ribbon.Style then
    begin
    {$IFDEF EXPRESSSKINS}
      if ASkin is TdxSkinRibbonPainter then
      begin
        if not TdxSkinRibbonPainter(ASkin).Painter.IsInternalPainter and
          TdxSkinRibbonPainter(ASkin).Painter.GetPainterDetails(ASkinDetails)
        then
          scgiLookAndFeel.AddSkin(ASkinDetails);
      end
      else
    {$ENDIF}
        if GetGlyphIndex(ASkin.Name, AIndex) then
        begin
          ASkinItem := scgiLookAndFeel.AddSkin(ASkin.Name, RibbonColorSchemesGroupName);
          ASkinItem.Caption := DisplayNameMap[AIndex];
          SetGlyph(ASkinItem.GlyphInDropDown, ilLargeColorSchemesGlyphs, AIndex);
          SetGlyph(ASkinItem.Glyph, ilSmallColorSchemesGlyphs, AIndex);
        end;
    end;
  end;
end;

procedure TfrmRichEditControlBase.scgiLookAndFeelSelected(Sender: TObject;
  const ASkinName: string);
begin
  SetColorScheme(ASkinName);
end;

procedure TfrmRichEditControlBase.SetColorScheme(const AName: string);
var
  ASkinName: string;
begin
  scgiLookAndFeel.SelectedSkinName := AName;
  Ribbon.ColorSchemeName := AName;

  ASkinName := AName;
  if cxLookAndFeelPaintersManager.GetPainter(AName) = nil then
    if Ribbon.ColorScheme.Style = rs2013 then
      ASkinName := 'Office2013White';

  cxLookAndFeelController.BeginUpdate;
  try
    cxLookAndFeelController.SkinName := ASkinName;
    cxLookAndFeelController.NativeStyle := cxLookAndFeelPaintersManager.GetPainter(ASkinName) = nil;
  finally
    cxLookAndFeelController.EndUpdate;
  end;
end;

procedure TfrmRichEditControlBase.SetRibbonDemoStyle(const AStyle: TRibbonDemoStyle);
const
  NamesMap: array[TdxRibbonStyle] of string = (
    'RIBBONAPPGLYPH', 'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010',
    'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010'
  );
begin
  Ribbon.Style := RibbonDemoStyleToRibbonStyle[AStyle];
  if AStyle in [rdsOffice2007, rdsScenic] then
  begin
    Ribbon.EnableTabAero := False;
    Ribbon.ApplicationButton.Menu := ApplicationMenu;
  end
  else
    Ribbon.EnableTabAero := True;
  Ribbon.ApplicationButton.Glyph.LoadFromResource(HInstance, NamesMap[Ribbon.Style], RT_BITMAP);
  Ribbon.ApplicationButton.StretchGlyph := Ribbon.Style = rs2007;
  DisableAero := AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019];
  scgiLookAndFeel.PopulateGallery;
  SetColorScheme(Ribbon.ColorSchemeName);
end;

end.
