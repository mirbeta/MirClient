unit RibbonNotepadMainForm;

interface

{$I cxVer.Inc}
{$R RibbonNotepadDemoAppGlyphs.res}

uses
{$IFDEF EXPRESSSKINS}
  dxSkinsdxRibbonPainter,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, IniFiles,
  Dialogs, NotepadMainForm, cxFontNameComboBox, cxDropDownEdit, cxPC, ActnList, dxTabbedMDI, dxBar, dxRibbonGallery,
  dxSkinChooserGallery, cxBarEditItem, cxLookAndFeels, ImgList, cxGraphics, cxControls, cxLookAndFeelPainters,
  dxRibbonSkins, cxClasses, dxRibbon, dxStatusBar, dxRibbonStatusBar, dxScreenTip, dxBarApplicationMenu, dxBarExtItems,
  cxContainer, cxEdit, cxTrackBar, dxZoomTrackBar, cxLabel, NotepadChildForm, Menus, dxGDIPlusClasses, ExtCtrls,
  cxTextEdit, cxMemo, StdCtrls, cxButtons, cxScrollBox, dxGallery, dxGalleryControl, dxBevel, cxGroupBox,
  dxRibbonBackstageViewGalleryControl, dxRibbonBackstageView, RibbonNotepadDemoOptions, RibbonNotepadChildForm,
  dxRibbonMiniToolbar, dxRibbonRadialMenu, dxColorDialog, dxBarBuiltInMenu, ExtDlgs,
  cxMaskEdit, dxRibbonColorGallery, cxImageList, dxOfficeSearchBox, dxCore;

const
  SchemeColorCount = 10;

type

  { TColorPickerController }

  TColorMap = array [0..SchemeColorCount-1] of TColor;

  TColorPickerController = class
  private
    FColorGallery: TdxRibbonColorGalleryItem;
    FColorGlyphSize: Integer;
    FColorMapItem: TdxRibbonGalleryItem;
  protected
    function CreateColorBitmap(AColor: TColor; AGlyphSize: Integer = 0): TcxAlphaBitmap;
    procedure BuildColorSchemeGallery;
    procedure ColorMapChanged;
    procedure ColorMapItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
  public
    constructor Create(AColorMapItem: TdxRibbonGalleryItem; AColorGallery: TdxRibbonColorGalleryItem; ARibbon: TdxRibbon);
  end;

  { TdxRibbonRecentDocumentsController }

  TdxRibbonRecentDocumentsController = class(TRecentDocumentsController)
  private
    FApplicationMenu: TdxBarApplicationMenu;
    FCurrentFolder: TdxRibbonBackstageViewGalleryControl;
    FRecentDocuments: TdxRibbonBackstageViewGalleryControl;
    FRecentPaths: TdxRibbonBackstageViewGalleryControl;

    function GetItemByValue(AGroup: TdxRibbonBackstageViewGalleryGroup; const AValue: string): TdxRibbonBackstageViewGalleryItem;
    function InternalAdd(AGroup: TdxRibbonBackstageViewGalleryGroup; const AValue: string): TdxRibbonBackstageViewGalleryItem;
    procedure InternalLoad(AGroup: TdxRibbonBackstageViewGalleryGroup; AIniFile: TCustomIniFile; const ASection: string);
    procedure InternalSave(AGroup: TdxRibbonBackstageViewGalleryGroup; AIniFile: TCustomIniFile; const ASection: string);
    procedure UpdateApplicationMenu;
  protected
    procedure DoLoad(AConfig: TCustomIniFile); override;
    procedure DoSave(AConfig: TCustomIniFile); override;
  public
    constructor Create(ARecentDocuments, ARecentPaths, ACurrentFolder: TdxRibbonBackstageViewGalleryControl; AApplicationMenu: TdxBarApplicationMenu);
    procedure Add(const AFileName: string); override;
    procedure SetCurrentFileName(const AFileName: string); override;
  end;

  { TfrmRibbonNotepadMain }

  TfrmRibbonNotepadMain = class(TfrmNotepadMain)
    acQATAboveRibbon: TAction;
    acQATBelowRibbon: TAction;
    ApplicationMenu: TdxBarApplicationMenu;
    BackstageView: TdxRibbonBackstageView;
    bbAlignCenter: TdxBarLargeButton;
    bbAlignLeft: TdxBarLargeButton;
    bbAlignRight: TdxBarLargeButton;
    bbApplicationButton: TdxBarLargeButton;
    bbBarsHelp: TdxBarLargeButton;
    bbBold: TdxBarLargeButton;
    bbBullets: TdxBarLargeButton;
    bbClear: TdxBarLargeButton;
    bbClearImage: TdxBarLargeButton;
    bbCopy: TdxBarLargeButton;
    bbCut: TdxBarLargeButton;
    bbDockingHelp: TdxBarLargeButton;
    bbDXDownloads: TdxBarLargeButton;
    bbDXOnWeb: TdxBarLargeButton;
    bbDXProducts: TdxBarLargeButton;
    bbDXSupport: TdxBarLargeButton;
    bbExit: TdxBarButton;
    bbFind: TdxBarLargeButton;
    bbFont: TdxBarButton;
    bbItalic: TdxBarLargeButton;
    bbLoadImage: TdxBarLargeButton;
    bbLogo: TdxBarButton;
    bbMiniToolbar: TdxBarLargeButton;
    bbMyDX: TdxBarLargeButton;
    bbNew: TdxBarLargeButton;
    bbOpen: TdxBarLargeButton;
    bbOptions: TdxBarButton;
    bbPaste: TdxBarLargeButton;
    bbPrint: TdxBarLargeButton;
    bbQATAboveRibbon: TdxBarButton;
    bbQATBelowRibbon: TdxBarButton;
    bbQATVisible: TdxBarLargeButton;
    bbRadialMenu: TdxBarLargeButton;
    bbReplace: TdxBarLargeButton;
    bbRibbonForm: TdxBarLargeButton;
    bbSave: TdxBarLargeButton;
    bbSaveAsRTF: TdxBarLargeButton;
    bbSaveAsText: TdxBarLargeButton;
    bbSelectAll: TdxBarLargeButton;
    bbTouchMode: TdxBarLargeButton;
    bbUnderline: TdxBarLargeButton;
    bbUndo: TdxBarLargeButton;
    bbUndoAll: TdxBarButton;
    bccZoom: TdxBarControlContainerItem;
    bsepSaveAs: TdxBarSeparator;
    bsSave: TdxBarSubItem;
    bsSaveAs: TdxBarSubItem;
    bsSelectionInfo: TdxBarStatic;
    bsZoom: TdxBarStatic;
    btnBrowsePath: TcxButton;
    btnSpecifyImage: TcxButton;
    bvgcCurrentFolder: TdxRibbonBackstageViewGalleryControl;
    bvgcLocations: TdxRibbonBackstageViewGalleryControl;
    bvgcLocationsComputerItem: TdxRibbonBackstageViewGalleryItem;
    bvgcLocationsGroup1: TdxRibbonBackstageViewGalleryGroup;
    bvgcLocationsRecentDocumentsGroup: TdxRibbonBackstageViewGalleryGroup;
    bvgcLocationsRecentDocumentsItem: TdxRibbonBackstageViewGalleryItem;
    bvgcRecentDocuments: TdxRibbonBackstageViewGalleryControl;
    bvgcRecentPaths: TdxRibbonBackstageViewGalleryControl;
    bvgcRecentPathsGroup: TdxRibbonBackstageViewGalleryGroup;
    bvtsAbout: TdxRibbonBackstageViewTabSheet;
    bvtsHelp: TdxRibbonBackstageViewTabSheet;
    bvtsOpen: TdxRibbonBackstageViewTabSheet;
    bvtsOptions: TdxRibbonBackstageViewTabSheet;
    bvtsSaveAs: TdxRibbonBackstageViewTabSheet;
    cbColorScheme: TcxComboBox;
    cbColorSchemeAccent: TcxComboBox;
    cbRibbonStyle: TcxComboBox;
    cbScreenTipStyle: TcxComboBox;
    cgiFontColor: TdxRibbonColorGalleryItem;
    cxLabel2: TcxLabel;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarPopupMenu: TdxRibbonPopupMenu;
    dxBarScreenTipRepository1: TdxBarScreenTipRepository;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxbClipboard: TdxBar;
    dxbColorScheme: TdxBar;
    dxbContextMenuStyle: TdxBar;
    dxbEditing: TdxBar;
    dxBevel1: TdxBevel;
    dxbFile: TdxBar;
    dxbFontAndColors: TdxBar;
    dxbHelp: TdxBar;
    dxbLinks: TdxBar;
    dxbParagraph: TdxBar;
    dxbQAT: TdxBar;
    dxbQATOptions: TdxBar;
    dxbRibbonOptions: TdxBar;
    dxbSelectionTools: TdxBar;
    dxbStatusBarToolbar1: TdxBar;
    dxbStatusBarToolbar2: TdxBar;
    dxbStatusBarToolbar3: TdxBar;
    dxbTabAreaToolbar: TdxBar;
    dxRibbonGalleryItem1Group1: TdxRibbonGalleryGroup;
    dxRibbonGalleryItem1Group2: TdxRibbonGalleryGroup;
    dxRibbonGalleryItem1Group3: TdxRibbonGalleryGroup;
    dxRibbonGalleryItem1Group4: TdxRibbonGalleryGroup;
    dxStatusBar: TdxRibbonStatusBar;
    gbBackstageViewTabCaption: TcxGroupBox;
    gbColorScheme: TcxGroupBox;
    gbColorSchemeAccent: TcxGroupBox;
    gbHelpContent: TcxGroupBox;
    gbLocationsMain: TcxGroupBox;
    gbLocationsPane: TcxGroupBox;
    gbOptionsPane: TcxGroupBox;
    gbPersonalizationOptions: TcxGroupBox;
    gbRecentDocumentsPane: TcxScrollBox;
    gbRecentPathsPane: TcxScrollBox;
    gbRecentPathsPaneBottom: TcxGroupBox;
    gbRecentPathsPaneCurrentFolder: TcxGroupBox;
    gbRibbonBackgroundImageInfo: TcxGroupBox;
    gbRibbonBackgroundImagePane: TcxGroupBox;
    gbRibbonStyle: TcxGroupBox;
    gbScreenTipStyle: TcxGroupBox;
    gbUserInterfaceOptions: TcxGroupBox;
    ilLargeColorSchemesGlyphs: TcxImageList;
    ilSmallColorSchemesGlyphs: TcxImageList;
    Image1: TImage;
    imDot1: TImage;
    imDot2: TImage;
    imDot3: TImage;
    imDot4: TImage;
    imDot5: TImage;
    imDot6: TImage;
    imLogo: TImage;
    lbbvTabCaption2010: TcxLabel;
    lbbvTabCaption2013: TcxLabel;
    lbColorScheme: TcxLabel;
    lbColorSchemeAccent: TcxLabel;
    lbComputer: TcxLabel;
    lbCurrentFolder: TcxLabel;
    lblClientCenter: TcxLabel;
    lblDownloads: TcxLabel;
    lblDXonWeb: TcxLabel;
    lblHelpBars: TcxLabel;
    lblHelpDocking: TcxLabel;
    lblProducts: TcxLabel;
    lblSupport: TcxLabel;
    lblSupportCenter: TcxLabel;
    lbPersonalizationOptions: TcxLabel;
    lbRecentDocuments: TcxLabel;
    lbRecentFolders: TcxLabel;
    lbRibbonBackgroundImageCaption: TcxLabel;
    lbRibbonBackgroundImageDescription: TcxLabel;
    lbRibbonStyle: TcxLabel;
    lbScreenTipStyle: TcxLabel;
    lbUserInterfaceOptions: TcxLabel;
    meAbout: TcxMemo;
    MiniToolbar: TdxRibbonMiniToolbar;
    OpenPictureDialog: TOpenPictureDialog;
    ppmRibbonBackgroundImage: TdxRibbonPopupMenu;
    rgiColorTheme: TdxRibbonGalleryItem;
    rgiFontColor: TdxRibbonGalleryItem;
    rgiItemSymbol: TdxRibbonGalleryItem;
    rgiUndo: TdxRibbonGalleryItem;
    Ribbon: TdxRibbon;
    RibbonRadialMenu: TdxRibbonRadialMenu;
    rtAppearance: TdxRibbonTab;
    rtHelp: TdxRibbonTab;
    rtSelection: TdxRibbonTab;
    stAlignCenter: TdxBarScreenTip;
    stAlignLeft: TdxBarScreenTip;
    stAlignRight: TdxBarScreenTip;
    stAppButton: TdxBarScreenTip;
    stAppMenu: TdxBarScreenTip;
    stBlack: TdxBarScreenTip;
    stBlue: TdxBarScreenTip;
    stBold: TdxBarScreenTip;
    stBullets: TdxBarScreenTip;
    stCopy: TdxBarScreenTip;
    stCut: TdxBarScreenTip;
    stFind: TdxBarScreenTip;
    stFontDialog: TdxBarScreenTip;
    stHelpButton: TdxBarScreenTip;
    stItalic: TdxBarScreenTip;
    stNew: TdxBarScreenTip;
    stOpen: TdxBarScreenTip;
    stPaste: TdxBarScreenTip;
    stPrint: TdxBarScreenTip;
    stQAT: TdxBarScreenTip;
    stQATAbove: TdxBarScreenTip;
    stQATBelow: TdxBarScreenTip;
    stReplace: TdxBarScreenTip;
    stRibbonForm: TdxBarScreenTip;
    stSilver: TdxBarScreenTip;
    stUnderline: TdxBarScreenTip;
    tabHome: TdxRibbonTab;
    tbZoom: TdxZoomTrackBar;
    UndoDropDownGallery: TdxRibbonDropDownGallery;
    dxbTabAreaSearchToolbar: TdxBar;
    beOfficeSearchBox: TcxBarEditItem;
    dxbSearchOptions: TdxBar;
    bbRecursiveSearch: TdxBarLargeButton;
    bbShowPaths: TdxBarLargeButton;

    procedure acQATAboveRibbonUpdate(Sender: TObject);
    procedure acQATBelowRibbonExecute(Sender: TObject);
    procedure BackstageViewPopup(Sender: TObject);
    procedure BackstageViewTabChanged(Sender: TObject);
    procedure bbApplicationButtonClick(Sender: TObject);
    procedure bbClearImageClick(Sender: TObject);
    procedure bbLoadImageClick(Sender: TObject);
    procedure bbOptionsClick(Sender: TObject);
    procedure bbQATVisibleClick(Sender: TObject);
    procedure bbRecursiveSearchClick(Sender: TObject);
    procedure bbRibbonFormClick(Sender: TObject);
    procedure bbSaveAsRTFClick(Sender: TObject);
    procedure bbSaveAsTextClick(Sender: TObject);
    procedure bbShowPathsClick(Sender: TObject);
    procedure bbTouchModeClick(Sender: TObject);
    procedure bbUndoAllClick(Sender: TObject);
    procedure btnBrowsePathClick(Sender: TObject);
    procedure bvgcLocationsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure bvgcRecentDocumentsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure bvgcRecentPathsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure cbColorSchemeAccentPropertiesChange(Sender: TObject);
    procedure cbColorSchemePropertiesChange(Sender: TObject);
    procedure cbRibbonStylePropertiesChange(Sender: TObject);
    procedure cbScreenTipStylePropertiesChange(Sender: TObject);
    procedure cgiFontColorColorChanged(Sender: TObject);
    procedure dxbFontAndColorsCaptionButtons0Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgiItemSymbolGroupItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
    procedure rgiUndoGroupItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
    procedure rgiUndoHotTrackedItemChanged(APrevHotTrackedGroupItem, ANewHotTrackedGroupItem: TdxRibbonGalleryGroupItem);
    procedure scgiLookAndFeelPopulate(Sender: TObject);
    procedure scgiLookAndFeelSelected(Sender: TObject; const ASkinName: string);
    procedure tbZoomPropertiesChange(Sender: TObject);
    procedure UndoDropDownGalleryPopup(Sender: TObject);
    procedure ApplicationMenuExtraPaneItemClick(Sender: TObject; AIndex: Integer);
  private
    FColorPickerController: TColorPickerController;
    function GetRibbonDemoStyle: TRibbonDemoStyle;
    procedure SetColorScheme(const AName: string);
    procedure SetRibbonDemoStyle(AStyle: TRibbonDemoStyle);

    procedure AssignFontColorGlyph;
    procedure EditorContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure InitOptions;
    procedure InitSymbolGallery;
    procedure UpdateColorSchemeRelatedControls;
    procedure UpdateImageIndexes;
    procedure UpdateOptionsVisibility;
  protected
    function CreateChildForm: TfrmNotepadChild; override;
    function CreateRecentDocumentsController: TRecentDocumentsController; override;
    procedure DoUpdateControls(AActiveChild: TfrmNotepadChild); override;
    procedure InitializeLookAndFeel; override;
    procedure UpdateUndoRelatedControls; override;
  public
    property RibbonDemoStyle: TRibbonDemoStyle read GetRibbonDemoStyle write SetRibbonDemoStyle;
  end;

var
  frmRibbonNotepadMain: TfrmRibbonNotepadMain;

implementation

uses
  dxSkinsCore, Clipbrd, dxOffice11, cxGeometry, RibbonNotepadDemoGallerySetup, dxBarSkinConsts, EBarsUtils, Math,
  dxCoreGraphics, dxDPIAwareUtils;

{$R *.dfm}

type
  TAccent = (aLight80, aLight60, aLight50, aLight40, aLight35, aLight25, aLight15, aLight5, aDark10, aDark25, aDark50, aDark75, aDark90);

  TClipboardAccess = class(TClipboard);

  TColorMapInfo = record
    Name: string;
    Map: TColorMap;
  end;

  TStringsArray = array of string;

const
  StandardColorMap: TColorMap =
    ($0000C0, $0000FF, $00C0FF, $00FFFF, $50D092, $50B000, $F0B000, $C07000, $602000, $A03070);

  ColorMaps: array [0..5] of TColorMapInfo =(
    (Name: 'Default'; Map: (clWindow, clWindowText, $D2B48C, $00008B, $0000FF, $FF0000, $556B2F, $800080, clAqua, $FFA500)),
    (Name: 'Theme1'; Map: (clWindow, clWindowText, $7D491F, $E1ECEE, $BD814F, $4D50C0, $59BB9B, $A26480, $C6AC4B, $4696F7)),
    (Name: 'Theme2'; Map: (clWindow, clWindowText, $6D6769, $D1C2C9, $66B9CE, $84B09C, $C9B16B, $CF8565, $C96B7E, $BB79A3)),
    (Name: 'Theme3'; Map: (clWindow, clWindowText, $323232, $D1DEE3, $097FF0, $36299F, $7C581B, $42854E, $784860, $5998C1)),
    (Name: 'Theme4'; Map: (clWindow, clWindowText, $866B64, $D7D1C5, $4963D1, $00B4CC, $AEAD8C, $707B8C, $8CB08F, $4990D1)),
    (Name: 'Theme5'; Map: (clWindow, clWindowText, $464646, $FAF5DE, $BFA22D, $281FDA, $1B64EB, $9D6339, $784B47, $4A3C7D))
  );

function DPIRatio: Single;
begin
  Result := Screen.PixelsPerInch / 96;
end;

{ TColorPickerController }

constructor TColorPickerController.Create(AColorMapItem: TdxRibbonGalleryItem;
  AColorGallery: TdxRibbonColorGalleryItem; ARibbon: TdxRibbon);
begin
  inherited Create;
  FColorGallery := AColorGallery;
  FColorGlyphSize := cxTextHeight(ARibbon.Fonts.Group);

  FColorMapItem := AColorMapItem;
  FColorMapItem.GalleryOptions.ColumnCount := 1;
  FColorMapItem.GalleryOptions.SpaceBetweenItemsAndBorder := 0;
  FColorMapItem.GalleryOptions.ItemTextKind := itkCaption;
  FColorMapItem.GalleryGroups.Add;
  FColorMapItem.OnGroupItemClick := ColorMapItemClick;

  BuildColorSchemeGallery;
end;

function TColorPickerController.CreateColorBitmap(AColor: TColor; AGlyphSize: Integer): TcxAlphaBitmap;
begin
  if AGlyphSize = 0 then
    AGlyphSize := FColorGlyphSize;
  Result := TcxAlphaBitmap.CreateSize(AGlyphSize, AGlyphSize);
  FillRectByColor(Result.Canvas.Handle, Result.ClientRect, AColor);
  FrameRectByColor(Result.Canvas.Handle, Result.ClientRect, clGray);
  if AColor = clNone then
    Result.RecoverAlphaChannel(0)
  else
    Result.TransformBitmap(btmSetOpaque);
end;

procedure TColorPickerController.BuildColorSchemeGallery;
const
  ASystemColorCount = 2;
  AGlyphOffset = 1;
var
  I, J: Integer;
  ABitmap, AColorBitmap: TcxAlphaBitmap;
  ARect: TRect;
  AGroupItem: TdxRibbonGalleryGroupItem;
  AThemeColorCount: Integer;
begin
  FColorMapItem.BarManager.BeginUpdate;
  try
    AThemeColorCount := SchemeColorCount - ASystemColorCount;
    ABitmap := TcxAlphaBitmap.CreateSize(FColorGlyphSize * AThemeColorCount + (AThemeColorCount - 1) * AGlyphOffset, FColorGlyphSize, True);
    try
      for I := High(ColorMaps) downto Low(ColorMaps) do
      begin
        AGroupItem := FColorMapItem.GalleryGroups[0].Items.Insert(0);
        for J := Low(ColorMaps[I].Map) + ASystemColorCount to High(ColorMaps[I].Map) do
        begin
          AColorBitmap := CreateColorBitmap(ColorMaps[I].Map[J]);
          try
            ARect := cxRectOffset(AColorBitmap.ClientRect, (AColorBitmap.Width + AGlyphOffset) * (J - ASystemColorCount), 0);
            ABitmap.CopyBitmap(AColorBitmap, ARect, cxNullPoint);
          finally
            AColorBitmap.Free;
          end;
        end;
        AGroupItem.Glyph.Assign(ABitmap);
        AGroupItem.Caption := ColorMaps[I].Name;
        AGroupItem.Tag := I;
      end;
      AGroupItem.Selected := True;
    finally
      ABitmap.Free;
    end;
  finally
    FColorMapItem.BarManager.EndUpdate;
  end;
end;

procedure TColorPickerController.ColorMapChanged;

  procedure FillGlyph(AGlyph: TcxAlphaBitmap);
  var
    ARect: TRect;
    ADC: HDC;
  begin
    ARect := Rect(0, 0, AGlyph.Width div 2, AGlyph.Height div 2);
    ADC := AGlyph.Canvas.Handle;
    FillRectByColor(ADC, ARect, ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[2]);
    FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), 0), ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[3]);
    FillRectByColor(ADC, cxRectOffset(ARect, 0, cxRectHeight(ARect)), ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[4]);
    FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), cxRectHeight(ARect)),
      ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[5]);
    FrameRectByColor(ADC, AGlyph.ClientRect, clGray);
    AGlyph.TransformBitmap(btmSetOpaque);
  end;

var
  AGlyph: TcxAlphaBitmap;
begin
  FColorMapItem.BarManager.BeginUpdate;
  try
    AGlyph := TcxAlphaBitmap.CreateSize(16, 16);
    FillGlyph(AGlyph);
    FColorMapItem.Glyph.Assign(AGlyph);
    AGlyph.SetSize(32, 32);
    FillGlyph(AGlyph);
    FColorMapItem.LargeGlyph.Assign(AGlyph);
    AGlyph.Free;
  finally
    FColorMapItem.BarManager.EndUpdate(False);
  end
end;

procedure TColorPickerController.ColorMapItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
begin
  FColorGallery.Theme := TdxRibbonColorGalleryTheme(AItem.Tag);
  ColorMapChanged;
end;

{ TdxRibbonRecentDocumentsController }

constructor TdxRibbonRecentDocumentsController.Create(
  ARecentDocuments, ARecentPaths, ACurrentFolder: TdxRibbonBackstageViewGalleryControl;
  AApplicationMenu: TdxBarApplicationMenu);
begin
  inherited Create;
  FApplicationMenu := AApplicationMenu;

  FCurrentFolder := ACurrentFolder;
  FCurrentFolder.Tag := 1;

  FRecentDocuments := ARecentDocuments;
  FRecentDocuments.Gallery.Groups.Add;
  FRecentDocuments.Tag := 0;

  FRecentPaths := ARecentPaths;
  FRecentPaths.Gallery.Groups.Add;
  FRecentPaths.Tag := 1;
end;

procedure TdxRibbonRecentDocumentsController.Add(const AFileName: string);
begin
  InternalAdd(FRecentDocuments.Gallery.Groups[0], AFileName);
  InternalAdd(FRecentPaths.Gallery.Groups[0], ExtractFileDir(AFileName));
  UpdateApplicationMenu;
end;

procedure TdxRibbonRecentDocumentsController.SetCurrentFileName(const AFileName: string);
begin
  FCurrentFolder.Gallery.Groups.BeginUpdate;
  try
    FCurrentFolder.Gallery.Groups.Clear;
    if ExtractFileDir(AFileName) <> '' then
      InternalAdd(FCurrentFolder.Gallery.Groups.Add, ExtractFileDir(AFileName));
  finally
    FCurrentFolder.Gallery.Groups.EndUpdate;
  end;
end;

procedure TdxRibbonRecentDocumentsController.DoLoad(AConfig: TCustomIniFile);
begin
  InternalLoad(FRecentDocuments.Gallery.Groups[0], AConfig, 'RecentDocuments');
  InternalLoad(FRecentPaths.Gallery.Groups[0], AConfig, 'RecentPaths');
  UpdateApplicationMenu;
end;

procedure TdxRibbonRecentDocumentsController.DoSave(AConfig: TCustomIniFile);
begin
  InternalSave(FRecentDocuments.Gallery.Groups[0], AConfig, 'RecentDocuments');
  InternalSave(FRecentPaths.Gallery.Groups[0], AConfig, 'RecentPaths');
end;

function TdxRibbonRecentDocumentsController.GetItemByValue(
  AGroup: TdxRibbonBackstageViewGalleryGroup; const AValue: string): TdxRibbonBackstageViewGalleryItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AGroup.Items.Count - 1 do
    if SameText(AGroup.Items[I].Hint, AValue) then
    begin
      Result := AGroup.Items[I];
      Break;
    end;
end;

function TdxRibbonRecentDocumentsController.InternalAdd(
  AGroup: TdxRibbonBackstageViewGalleryGroup; const AValue: string): TdxRibbonBackstageViewGalleryItem;
begin
  Result := GetItemByValue(AGroup, AValue);
  if Result = nil then
  begin
    Result := AGroup.Items.Add;
    Result.Caption := ExtractFileName(AValue);
    Result.Hint := AValue;
    Result.Description := StringReplace(AValue, '\', ' '#187' ', [rfReplaceAll]);
    Result.ImageIndex := AGroup.GetParentComponent.Tag;
  end;
  Result.Index := 0;

  while AGroup.Items.Count > 10 do
    AGroup.Items.Delete(9);
end;

procedure TdxRibbonRecentDocumentsController.InternalLoad(
  AGroup: TdxRibbonBackstageViewGalleryGroup; AIniFile: TCustomIniFile; const ASection: string);
var
  AItem: TdxRibbonBackstageViewGalleryItem;
  I: Integer;
begin
  AGroup.Items.BeginUpdate;
  try
    AGroup.Items.Clear;
    for I := 0 to AIniFile.ReadInteger(ASection, 'Count', 0) - 1 do
    begin
      AItem := InternalAdd(AGroup, AIniFile.ReadString(ASection, IntToStr(I), ''));
      AItem.Pinned := AIniFile.ReadBool(ASection, IntToStr(I) + 'Pinned', False);
    end;
  finally
    AGroup.Items.EndUpdate;
  end;
end;

procedure TdxRibbonRecentDocumentsController.InternalSave(
  AGroup: TdxRibbonBackstageViewGalleryGroup; AIniFile: TCustomIniFile; const ASection: string);
var
  AItem: TdxRibbonBackstageViewGalleryItem;
  I: Integer;
begin
  AIniFile.EraseSection(ASection);
  AIniFile.WriteInteger(ASection, 'Count', AGroup.Items.Count);
  for I := 0 to AGroup.Items.Count - 1 do
  begin
    AItem := AGroup.Items[I];
    AIniFile.WriteString(ASection, IntToStr(I), AItem.Hint);
    AIniFile.WriteBool(ASection, IntToStr(I) + 'Pinned', AItem.Pinned);
  end;
end;

procedure TdxRibbonRecentDocumentsController.UpdateApplicationMenu;
var
  AGroup: TdxRibbonBackstageViewGalleryGroup;
  AItem: TdxBarExtraPaneItem;
  I: Integer;
begin
  if FApplicationMenu = nil then
    Exit;

  FApplicationMenu.ExtraPaneItems.BeginUpdate;
  try
    FApplicationMenu.ExtraPaneItems.Clear;
    AGroup := FRecentDocuments.Gallery.Groups[0];
    for I := 0 to AGroup.ItemCount - 1 do
    begin
      AItem := FApplicationMenu.ExtraPaneItems.Add;
      AItem.DisplayText := AGroup.Items[I].Caption;
      AItem.Pin := AGroup.Items[I].Pinned;
      AItem.Text := AGroup.Items[I].Hint;
    end;
  finally
    FApplicationMenu.ExtraPaneItems.EndUpdate;
  end;
end;


{ TfrmRibbonNotepadMain }

function TfrmRibbonNotepadMain.GetRibbonDemoStyle: TRibbonDemoStyle;
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

procedure TfrmRibbonNotepadMain.SetRibbonDemoStyle(AStyle: TRibbonDemoStyle);
const
  NamesMap: array[TdxRibbonStyle] of string = (
    'RIBBONAPPGLYPH', 'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010',
    'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010', 'RIBBONAPPGLYPH2010'
  );
begin
  if RibbonDemoStyle = AStyle then
    Exit;

  Ribbon.Style := RibbonDemoStyleToRibbonStyle[AStyle];
  if AStyle in [rdsOffice2007, rdsScenic] then
  begin
    Ribbon.EnableTabAero := False;
    Ribbon.ApplicationButton.Menu := ApplicationMenu;
    rtHelp.Visible := True;
  end
  else
  begin
    Ribbon.ApplicationButton.Menu := BackstageView;
    Ribbon.EnableTabAero := True;
    rtHelp.Visible := False;
  end;

  Ribbon.ApplicationButton.Glyph.LoadFromResource(HInstance, NamesMap[Ribbon.Style], RT_BITMAP);
  Ribbon.ApplicationButton.Glyph.SourceDPI := dxDefaultDPI;
  Ribbon.ApplicationButton.StretchGlyph := Ribbon.Style = rs2007;
  Ribbon.TabAreaToolbar.Visible := Ribbon.Style >= rs2016;
  Ribbon.TabAreaSearchToolbar.Visible := Ribbon.TabAreaToolbar.Visible;
  dxbSearchOptions.Visible := Ribbon.TabAreaSearchToolbar.Visible;
  lbbvTabCaption2010.Visible := AStyle = rdsOffice2010;
  lbbvTabCaption2013.Visible := AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019];
  
  if AStyle = rdsOffice2016Tablet then
    bbQATAboveRibbon.Visible := ivNever
  else
    bbQATAboveRibbon.Visible := ivAlways;
  bbQATBelowRibbon.Visible := bbQATAboveRibbon.Visible;

  if AStyle = rdsOffice2007 then
    bsepSaveAs.Visible := ivAlways
  else
    bsepSaveAs.Visible := ivNever;

  if AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019] then
    gbBackstageViewTabCaption.Height := cxTextHeight(lbbvTabCaption2013.Style.Font) + gbLocationsMain.Margins.Bottom
  else
    gbBackstageViewTabCaption.Height := cxTextHeight(lbbvTabCaption2010.Style.Font) + gbLocationsMain.Margins.Bottom;

  DisableAero := AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019];
  scgiLookAndFeel.PopulateGallery;
  SetColorScheme(Ribbon.ColorSchemeName);
  UpdateOptionsVisibility;
end;

procedure TfrmRibbonNotepadMain.SetColorScheme(const AName: string);
var
  ASkinName: string;
begin
  scgiLookAndFeel.SelectedSkinName := AName;
  Ribbon.ColorSchemeName := AName;
  UpdateColorSchemeRelatedControls;

  ASkinName := AName;
  if cxLookAndFeelPaintersManager.GetPainter(AName) = nil then
  begin
    if Ribbon.ColorScheme.Style = rs2013 then
      ASkinName := 'Office2013White';
  end;

  cxLookAndFeelController.BeginUpdate;
  try
    cxLookAndFeelController.SkinName := ASkinName;
    cxLookAndFeelController.NativeStyle := cxLookAndFeelPaintersManager.GetPainter(ASkinName) = nil;
  finally
    cxLookAndFeelController.EndUpdate;
  end;
end;

procedure TfrmRibbonNotepadMain.InitializeLookAndFeel;
begin
  RibbonDemoStyle := rdsOffice2019;
end;

procedure TfrmRibbonNotepadMain.UpdateUndoRelatedControls;
begin
  inherited UpdateUndoRelatedControls;
  bbUndo.Enabled := acUndo.Enabled;
  bbUndoAll.Enabled := acUndo.Enabled;
end;

procedure TfrmRibbonNotepadMain.InitOptions;
begin
  TRibbonDemoOptionsForm.PopulateRibbonStyles(cbRibbonStyle.Properties.Items);
  TRibbonDemoOptionsForm.PopulateColorSchemeAccents(cbColorSchemeAccent.Properties.Items);

  cbRibbonStyle.ItemIndex := Ord(RibbonDemoStyle);
  cbRibbonStylePropertiesChange(nil);
  cbColorScheme.ItemIndex := Max(0, cbColorScheme.Properties.Items.IndexOf(Ribbon.ColorSchemeName));
  cbColorSchemeAccent.ItemIndex := Ord(Ribbon.ColorSchemeAccent);
  if dxBarManager.ShowHint then
    cbScreenTipStyle.ItemIndex := Ord(not dxBarScreenTipRepository1.ShowDescription)
  else
    cbScreenTipStyle.ItemIndex := 2;

  bbClearImage.Enabled := not Ribbon.BackgroundImage.Empty;
  OpenPictureDialog.DefaultExt := GraphicExtension(TGraphicClass(Ribbon.BackgroundImage.ClassType));
  OpenPictureDialog.Filter := GraphicFilter(TGraphicClass(Ribbon.BackgroundImage.ClassType));
end;

procedure TfrmRibbonNotepadMain.InitSymbolGallery;

  procedure AddItem(AGroup: TdxRibbonGalleryGroup; ACode: Integer);

    function CreateBitmap(const AFont: string; AChar: WideChar): TcxAlphaBitmap;
    var
      AGlyphSize: Integer;
      R: TRect;
    begin
      AGlyphSize := Round(32 * DPIRatio);
      R := Rect(0, 0, AGlyphSize, AGlyphSize);
      Result := TcxAlphaBitmap.CreateSize(R);
      Result.Canvas.Brush.Color := $FAFAFA;
      Result.Canvas.FillRect(R);
      Result.Canvas.Font.Name := AFont;
      Result.Canvas.Font.Color := $5C534C;
      Result.Canvas.Font.Size := Round(16 * DPIRatio);
      DrawTextW(Result.Canvas.Handle, @AChar, 1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      Result.TransformBitmap(btmSetOpaque);
    end;

  var
    AItem: TdxRibbonGalleryGroupItem;
    AFont: string;
    ABitmap: TcxAlphaBitmap;
  begin
    AItem := AGroup.Items.Add;
    AFont := 'Times New Roman';
    AItem.Caption := AFont + ' #' + IntToStr(ACode);
    AItem.Description := AFont;
    AItem.Tag := ACode;

    ABitmap := CreateBitmap(AFont, WideChar(ACode));
    try
      AItem.Glyph.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;

  procedure PopulateGroup(AGroupIndex: Integer; AMap: array of Integer);
  var
    I: Integer;
  begin
    for I := Low(AMap) to High(AMap) do
      AddItem(rgiItemSymbol.GalleryGroups[AGroupIndex], AMap[I]);
  end;

const
  CurrencyMap: array [0..4] of Integer = ($20AC, $24, $A3, $A5, $20A3);
  GreekMap: array [0..9] of integer = ($03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA);
  MathMap: array [0..7] of Integer = ($B1, $2260, $2264, $2265, $F7, $D7, $221E, $2211);
  SymbolMap: array [0..2] of Integer = ($A9, $AE, $2122);
begin
  dxBarManager.BeginUpdate;
  try
    PopulateGroup(0, MathMap);
    PopulateGroup(1, GreekMap);
    PopulateGroup(2, SymbolMap);
    PopulateGroup(3, CurrencyMap);
  finally
    dxBarManager.EndUpdate;
  end;
end;

procedure TfrmRibbonNotepadMain.acQATAboveRibbonUpdate(Sender: TObject);
begin
  acQATAboveRibbon.Checked := Ribbon.QuickAccessToolbar.Position = qtpAboveRibbon;
  acQATBelowRibbon.Checked := Ribbon.QuickAccessToolbar.Position = qtpBelowRibbon;
end;

procedure TfrmRibbonNotepadMain.acQATBelowRibbonExecute(Sender: TObject);
begin
  if TAction(Sender).Tag <> 0 then
    Ribbon.QuickAccessToolbar.Position := qtpBelowRibbon
  else
    Ribbon.QuickAccessToolbar.Position := qtpAboveRibbon;
end;

procedure TfrmRibbonNotepadMain.ApplicationMenuExtraPaneItemClick(Sender: TObject; AIndex: Integer);
begin
  OpenFile(ApplicationMenu.ExtraPaneItems[AIndex].Text);
end;

procedure TfrmRibbonNotepadMain.AssignFontColorGlyph;
var
  AGlyph: TBitmap;
begin
  AGlyph := FColorPickerController.CreateColorBitmap(cgiFontColor.Color, ScaleFactor.Apply(16));
  try
    cgiFontColor.Glyph.Assign(AGlyph);
    cgiFontColor.Glyph.SourceDPI := ScaleFactor.Apply(96);
  finally
    AGlyph.Free;
  end;
end;

procedure TfrmRibbonNotepadMain.UndoDropDownGalleryPopup(Sender: TObject);
var
  AGroup: TdxRibbonGalleryGroup;
  I: Integer;
begin
  rgiUndo.GalleryCategories.BeginUpdate;
  try
    rgiUndo.GalleryCategories.Clear;
    AGroup := rgiUndo.GalleryCategories.Add;
    for I := 0 to ActiveChild.UndoController.Actions.Count - 1 do
      AGroup.Items.Add.Caption := ActiveChild.UndoController.Actions[I];
  finally
    rgiUndo.GalleryCategories.EndUpdate;
  end;
end;

procedure TfrmRibbonNotepadMain.UpdateColorSchemeRelatedControls;
var
  AColor: TColor;
begin
  AColor := Ribbon.ColorScheme.GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR);
  lbComputer.Style.TextColor := AColor;
  lbRecentDocuments.Style.TextColor := AColor;
  lbUserInterfaceOptions.Style.TextColor := AColor;
  lbRibbonStyle.Style.TextColor := AColor;
  lbScreenTipStyle.Style.TextColor := AColor;
  lbPersonalizationOptions.Style.TextColor := AColor;
  lbColorScheme.Style.TextColor := AColor;
  lbColorSchemeAccent.Style.TextColor := AColor;
  lbRibbonBackgroundImageCaption.Style.TextColor := AColor;
  lbRibbonBackgroundImageDescription.Style.TextColor := AColor;
  lbbvTabCaption2010.Style.TextColor := AColor;
  lbbvTabCaption2013.Style.TextColor := AColor;
  lbRecentFolders.Style.TextColor := AColor;
  lbCurrentFolder.Style.TextColor := AColor;
end;

procedure TfrmRibbonNotepadMain.BackstageViewPopup(Sender: TObject);
begin
  gbRecentPathsPaneCurrentFolder.Visible := bvgcCurrentFolder.Gallery.Groups.Count > 0;
  BackstageViewTabChanged(Sender);
end;

procedure TfrmRibbonNotepadMain.BackstageViewTabChanged(Sender: TObject);
begin
  if not (csReading in ComponentState) then
  begin
    gbBackstageViewTabCaption.Parent := BackstageView.ActiveTab;
    lbbvTabCaption2010.Caption := BackstageView.ActiveTab.Caption;
    lbbvTabCaption2013.Caption := BackstageView.ActiveTab.Caption;
    if bvtsOpen.Active or bvtsSaveAs.Active then
    begin
      bvgcLocationsRecentDocumentsGroup.Visible := BackstageView.ActiveTab = bvtsOpen;
      if bvgcLocationsRecentDocumentsGroup.Visible and not bvgcLocationsComputerItem.Checked then
        bvgcLocationsRecentDocumentsItem.Checked := True
      else
        bvgcLocationsComputerItem.Checked := True;

      gbLocationsMain.Parent := BackstageView.ActiveTab;
    end;
  end;
end;

procedure TfrmRibbonNotepadMain.bbApplicationButtonClick(Sender: TObject);
begin
  Ribbon.ApplicationButton.Visible := bbApplicationButton.Down;
end;

procedure TfrmRibbonNotepadMain.bbClearImageClick(Sender: TObject);
begin
  Ribbon.BackgroundImage.Clear;
  bbClearImage.Enabled := False;
end;

procedure TfrmRibbonNotepadMain.bbLoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    Ribbon.BackgroundImage.LoadFromFile(OpenPictureDialog.Filename);
    bbClearImage.Enabled := True;
  end;
end;

procedure TfrmRibbonNotepadMain.bbOptionsClick(Sender: TObject);
var
  AColorSchemeAccent: TdxRibbonColorSchemeAccent;
  AColorSchemeName: string;
  AScreenTipOptions: TScreenTipOptions;
  AStyle: TRibbonDemoStyle;
begin
  AStyle := RibbonDemoStyle;
  AColorSchemeName := Ribbon.ColorSchemeName;
  AColorSchemeAccent := Ribbon.ColorSchemeAccent;
  AScreenTipOptions.ShowScreenTips := dxBarManager.ShowHint;
  AScreenTipOptions.ShowDescripitons := dxBarScreenTipRepository1.ShowDescription;
  if ExecuteRibbonDemoOptions(AColorSchemeName, AScreenTipOptions, AStyle, AColorSchemeAccent) then
  begin
    RibbonDemoStyle := AStyle;
    Ribbon.ColorSchemeAccent := AColorSchemeAccent;
    dxBarManager.ShowHint := AScreenTipOptions.ShowScreenTips;
    dxBarScreenTipRepository1.ShowDescription := AScreenTipOptions.ShowDescripitons;
    SetColorScheme(AColorSchemeName);
    InitOptions;
  end;
end;

procedure TfrmRibbonNotepadMain.bbQATVisibleClick(Sender: TObject);
begin
  Ribbon.QuickAccessToolbar.Visible := bbQATVisible.Down;
end;

procedure TfrmRibbonNotepadMain.bbRecursiveSearchClick(Sender: TObject);
begin
  if bbRecursiveSearch.Down then
    (beOfficeSearchBox.Properties as TdxOfficeSearchBoxProperties).RecursiveSearch := bTrue
  else
    (beOfficeSearchBox.Properties as TdxOfficeSearchBoxProperties).RecursiveSearch := bFalse;
end;

procedure TfrmRibbonNotepadMain.bbRibbonFormClick(Sender: TObject);
begin
  Ribbon.SupportNonClientDrawing := bbRibbonForm.Down;
end;

procedure TfrmRibbonNotepadMain.bbSaveAsRTFClick(Sender: TObject);
begin
  ActiveChild.SaveFile(True);
end;

procedure TfrmRibbonNotepadMain.bbSaveAsTextClick(Sender: TObject);
begin
  ActiveChild.ExportAsPlainText;
end;

procedure TfrmRibbonNotepadMain.bbShowPathsClick(Sender: TObject);
begin
  (beOfficeSearchBox.Properties as TdxOfficeSearchBoxProperties).ShowResultPaths := bbShowPaths.Down;
end;

procedure TfrmRibbonNotepadMain.bbTouchModeClick(Sender: TObject);
var
  AHeight: Integer;
begin
  cxLookAndFeelController.TouchMode := bbTouchMode.Down;

  AHeight := tbZoom.Tag;
  dxAdjustToTouchableSize(AHeight);
  tbZoom.Height := AHeight;
end;

procedure TfrmRibbonNotepadMain.bbUndoAllClick(Sender: TObject);
begin
  ActiveChild.UndoController.Undo(MaxInt);
end;

procedure TfrmRibbonNotepadMain.btnBrowsePathClick(Sender: TObject);
var
  AHandled: Boolean;
begin
  if bvtsSaveAs.Active then
    AHandled := ActiveChild.SaveFile(True)
  else
    AHandled := OpenFile;

  if AHandled then
    BackstageView.Hide;
end;

procedure TfrmRibbonNotepadMain.bvgcLocationsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
begin
  gbRecentDocumentsPane.Visible := bvgcLocationsRecentDocumentsItem.Checked;
  gbRecentPathsPane.Visible := bvgcLocationsComputerItem.Checked;
end;

procedure TfrmRibbonNotepadMain.bvgcRecentDocumentsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
begin
  OpenFile(AItem.Hint);
  BackstageView.Hide;
end;

procedure TfrmRibbonNotepadMain.bvgcRecentPathsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
var
  AHandled: Boolean;
  APrevInitialDir: string;
begin
  if bvtsSaveAs.Active then
  begin
    APrevInitialDir := ActiveChild.SaveDialog.InitialDir;
    try
      ActiveChild.SaveDialog.InitialDir := AItem.Hint;
      AHandled := ActiveChild.SaveFile(True);
    finally
      ActiveChild.SaveDialog.InitialDir := APrevInitialDir;
    end;
  end
  else
  begin
    APrevInitialDir := OpenDialog.InitialDir;
    try
      OpenDialog.InitialDir := AItem.Hint;
      AHandled := OpenFile;
    finally
      OpenDialog.InitialDir := APrevInitialDir;
    end;
  end;
  if AHandled then
    BackstageView.Hide;
end;

procedure TfrmRibbonNotepadMain.cbColorSchemeAccentPropertiesChange(Sender: TObject);
begin
  Ribbon.ColorSchemeAccent := TdxRibbonColorSchemeAccent(cbColorSchemeAccent.ItemIndex);
  UpdateColorSchemeRelatedControls;
end;

procedure TfrmRibbonNotepadMain.cbColorSchemePropertiesChange(Sender: TObject);
begin
  SetColorScheme(cbColorScheme.Text);
end;

procedure TfrmRibbonNotepadMain.cbRibbonStylePropertiesChange(Sender: TObject);
var
  ASelectedColorSchemeName: string;
begin
  RibbonDemoStyle := TRibbonDemoStyle(cbRibbonStyle.ItemIndex);

  cbColorSchemeAccent.Enabled := cbRibbonStyle.ItemIndex > 0;
  lbColorSchemeAccent.Enabled := cbRibbonStyle.ItemIndex > 0;

  ASelectedColorSchemeName := cbColorScheme.Text;
  TRibbonDemoOptionsForm.PopulateColorSchemes(cbColorScheme.Properties.Items,
    RibbonDemoStyleToRibbonStyle[TRibbonDemoStyle(cbRibbonStyle.ItemIndex)]);
  cbColorScheme.ItemIndex := Max(0, cbColorScheme.Properties.Items.IndexOf(ASelectedColorSchemeName));
end;

procedure TfrmRibbonNotepadMain.cbScreenTipStylePropertiesChange(Sender: TObject);
begin
  dxBarManager.ShowHint := cbScreenTipStyle.ItemIndex <> 2;
  dxBarScreenTipRepository1.ShowDescription := cbScreenTipStyle.ItemIndex = 0;
end;

procedure TfrmRibbonNotepadMain.cgiFontColorColorChanged(Sender: TObject);
begin
  Editor.SelAttributes.Color := cgiFontColor.Color;
  ActiveChild.UndoController.AddAction(6);
  AssignFontColorGlyph;
end;

function TfrmRibbonNotepadMain.CreateChildForm: TfrmNotepadChild;
begin
  Result := TfrmRibbonNotepadChild.Create(Self);
  Result.Editor.OnContextPopup := EditorContextPopup;
  Result.Editor.OnMouseUp := EditorMouseUp;
end;

function TfrmRibbonNotepadMain.CreateRecentDocumentsController: TRecentDocumentsController;
begin
  Result := TdxRibbonRecentDocumentsController.Create(bvgcRecentDocuments, bvgcRecentPaths, bvgcCurrentFolder, ApplicationMenu);
end;

procedure TfrmRibbonNotepadMain.DoUpdateControls(AActiveChild: TfrmNotepadChild);
const
  SelectionContextIndex = 0;
begin
  inherited DoUpdateControls(AActiveChild);
  rgiItemSymbol.Enabled := AActiveChild <> nil;
  rgiColorTheme.Enabled := AActiveChild <> nil;
  cgiFontColor.Enabled := AActiveChild <> nil;
  bbUndo.Enabled := AActiveChild <> nil;
  tbZoom.Enabled := AActiveChild <> nil;
  bsZoom.Enabled := AActiveChild <> nil;

  if AActiveChild <> nil then
  begin
    tbZoom.Position := Round(100 * Editor.ActiveProperties.ZoomFactor);
    tbZoomPropertiesChange(nil);
  end;

  if (AActiveChild <> nil) and (Editor.SelLength > 0) then
    Ribbon.Contexts[SelectionContextIndex].Activate(False)
  else
    Ribbon.Contexts[SelectionContextIndex].Visible := False;
end;

procedure TfrmRibbonNotepadMain.UpdateImageIndexes;
begin
  bbBarsHelp.ImageIndex := 18;
  bbDockingHelp.ImageIndex := 18;
  dxBarLargeButton1.ImageIndex := 20;
  bbDXSupport.ImageIndex := 20;
  bbDXDownloads.ImageIndex := 20;
  bbDXOnWeb.ImageIndex := 20;
  bbDXProducts.ImageIndex := 20;
  bbMyDX.ImageIndex := 20;
end;

procedure TfrmRibbonNotepadMain.UpdateOptionsVisibility;
var
  AWasActive: Boolean;
begin
  AWasActive := bvtsOptions.Active;
  bvtsOptions.TabVisible := RibbonDemoStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet];
  if AWasActive and not bvtsOptions.TabVisible then
    bvtsOpen.Active := True;
  bbOptions.Visible := VisibleTodxBarVisible(not bvtsOptions.TabVisible);
  gbRibbonBackgroundImagePane.Visible := (DisableAero or not Ribbon.SupportNonClientDrawing) and
    (RibbonDemoStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet]);
end;

procedure TfrmRibbonNotepadMain.dxbFontAndColorsCaptionButtons0Click(Sender: TObject);
begin
  acFont.Execute;
end;

procedure TfrmRibbonNotepadMain.EditorContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if bbRadialMenu.Down then
    RibbonRadialMenu.PopupFromCursorPos
  else
    if Editor.SelLength <> 0 then
      MiniToolbar.Popup(dxBarPopupMenu)
    else
      dxBarPopupMenu.PopupFromCursorPos;

  Handled := True;
end;

procedure TfrmRibbonNotepadMain.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (Editor.SelLength <> 0) and bbMiniToolbar.Down then
    MiniToolbar.Popup;
end;

procedure TfrmRibbonNotepadMain.rgiItemSymbolGroupItemClick(
  Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);

  procedure InsertSymbol(AChar: WideChar);
  var
    S: WideString;
  begin
    Editor.SelAttributes.Name := AItem.Description;
    with TClipboardAccess(Clipboard) do
    begin
      Open;
      try
        S := AChar;
        SetBuffer(CF_UNICODETEXT, PWideChar(S)^, (Length(S) + 1) * SizeOf(WideChar));
      finally
        Close;
      end;
    end;
    Editor.PasteFromClipboard;
  end;

begin
  InsertSymbol(WideChar(AItem.Tag));
end;

procedure TfrmRibbonNotepadMain.rgiUndoGroupItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
begin
  ActiveChild.UndoController.Undo(AItem.Index + 1);
end;

procedure TfrmRibbonNotepadMain.rgiUndoHotTrackedItemChanged(
  APrevHotTrackedGroupItem, ANewHotTrackedGroupItem: TdxRibbonGalleryGroupItem);
var
  ACount: Integer;
  AString: string;
begin
  if ANewHotTrackedGroupItem <> nil then
  begin
    ACount := ANewHotTrackedGroupItem.Index + 1;
    if ACount = 1 then
      AString := ' Action'
    else
      AString := ' Actions';

    bsSelectionInfo.Caption := 'Undo ' + IntToStr(ACount) + AString;
  end
  else
    bsSelectionInfo.Caption := 'Cancel';
end;

procedure TfrmRibbonNotepadMain.scgiLookAndFeelPopulate(Sender: TObject);

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
    if (ASkin.Style = Ribbon.Style) and not ASkin.IsInternalPainter then
    begin
    {$IFDEF EXPRESSSKINS}
      if ASkin is TdxSkinRibbonPainter then
      begin
        if TdxSkinRibbonPainter(ASkin).Painter.GetPainterDetails(ASkinDetails) then
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

procedure TfrmRibbonNotepadMain.scgiLookAndFeelSelected(Sender: TObject; const ASkinName: string);
begin
  SetColorScheme(ASkinName);
  cbColorScheme.ItemIndex := Max(0, cbColorScheme.Properties.Items.IndexOf(Ribbon.ColorSchemeName));
end;

procedure TfrmRibbonNotepadMain.tbZoomPropertiesChange(Sender: TObject);
begin
  bsZoom.Caption := Format('%3d %%', [tbZoom.Position]);
  if FUpdatingControls = 0 then
    Editor.ActiveProperties.ZoomFactor := tbZoom.Position / 100;
end;

procedure TfrmRibbonNotepadMain.FormCreate(Sender: TObject);
var
  AAboutFileName: string;
  ATextWidth: Integer;
begin
  inherited;
  tbZoom.Tag := tbZoom.Height;
  FColorPickerController := TColorPickerController.Create(rgiColorTheme, cgiFontColor, Ribbon);

  AAboutFileName := ExtractFilePath(Application.ExeName) + 'About.txt';
  if FileExists(AAboutFileName) then
    meAbout.Lines.LoadFromFile(AAboutFileName);

  lblSupport.OnClick := dmCommonData.actSupport.OnExecute;
  lblHelpBars.OnClick := dmCommonData.actBarsHelp.OnExecute;
  lblHelpDocking.OnClick := dmCommonData.actDockingHelp.OnExecute;
  lblProducts.OnClick := dmCommonData.actProducts.OnExecute;
  lblClientCenter.OnClick := dmCommonData.actMyDX.OnExecute;
  lblDXonWeb.OnClick := dmCommonData.actDXOnTheWeb.OnExecute;
  lblDownloads.OnClick := dmCommonData.actDownloads.OnExecute;

  bbLogo.OnClick := dmCommonData.actDXOnTheWeb.OnExecute;

  ATextWidth := cxTextWidth(dxBarManager.Font, 'Undo 9999 Actions');
  rgiUndo.GalleryOptions.ItemPullHighlighting.Active := True;
  rgiUndo.GalleryOptions.ColumnCount := 1;
  rgiUndo.GalleryOptions.SubMenuResizing := gsrNone;
  rgiUndo.GalleryOptions.ItemSize.Width := ATextWidth;
  rgiUndo.GalleryOptions.ItemSize.Height := Max(cxTextHeight(dxBarManager.Font), Round(21 * DPIRatio));
  rgiUndo.GalleryGroups.Add;

  bsSelectionInfo.Width := ATextWidth;
  bsSelectionInfo.Caption := 'Cancel';

  bbRibbonForm.Down := Ribbon.SupportNonClientDrawing;
  bbTouchMode.Down := cxLookAndFeelController.TouchMode;
  bbApplicationButton.Down := Ribbon.ApplicationButton.Visible;
  bbQATVisible.Down := Ribbon.QuickAccessToolbar.Visible;
  AssignFontColorGlyph;
  InitSymbolGallery;
  UpdateImageIndexes;
  InitOptions;
end;

procedure TfrmRibbonNotepadMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FColorPickerController);
end;

end.
