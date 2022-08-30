unit EditorsStylesDemoNoteBook;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, EditorsStylesDemoBase, cxColorComboBox, cxDropDownEdit,
  cxFontNameComboBox, cxGroupBox, cxLabel, cxMemo, cxDBEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxCalendar, Menus,
  cxPropertiesStore, cxDBColorComboBox, DB, cxDBFontNameComboBox,
  cxNavigator, cxSplitter, ExtCtrls, cxSpinEdit, cxSpinButton, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDBData, cxClasses, cxImage,
  cxDBNavigator, cxLookAndFeels, cxLookAndFeelPainters, dxGalleryControl,
  dxColorGallery, dxDBColorGallery;

type
  TSpiralImageControl = class (TcxControl)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write FBitmap;
  end;

  TEditorsStylesDemoNoteBookFrame = class(TEditorsStylesDemoBaseFrame)
    cxGroupBox1: TcxGroupBox;
    lblNote: TcxLabel;
    cxGroupBox2: TcxGroupBox;
    cxGroupBox3: TcxGroupBox;
    pnSpiral: TPanel;
    dbmText: TcxDBMemo;
    gbDescriptionControls: TcxGroupBox;
    cxFontNameComboBox: TcxFontNameComboBox;
    cxSpinEdit: TcxSpinEdit;
    gbFont: TcxGroupBox;
    lbNoteFontColor: TcxLabel;
    cbFontName: TcxDBFontNameComboBox;
    cbFontColor: TcxDBColorComboBox;
    lbTextSize: TcxLabel;
    seTextSize: TcxDBSpinEdit;
    lbBkGround: TcxLabel;
    cxLabel2: TcxLabel;
    lblDate: TcxLabel;
    cxLabel1: TcxLabel;
    cxDBDateEdit1: TcxDBDateEdit;
    cxDBTextEdit1: TcxDBTextEdit;
    cxDBNavigator1: TcxDBNavigator;
    cxImage1: TcxImage;
    dxDBColorGallery1: TdxDBColorGallery;
    procedure dsNoteBookDataChange(Sender: TObject; Field: TField);
    procedure tblNoteBookAfterScroll(DataSet: TDataSet);
    procedure cxSpinEditPropertiesChange(Sender: TObject);
    procedure cxFontNameComboBoxPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSpiralImageControl: TSpiralImageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangeDisplayStyle(ADisplayStyle: TcxStyleSheetType); override;
    function ShowControlsAboveDescription: Boolean; override;
    function Name: string; override;
    function BriefName: string; override;
    function StylesIniPath: string; override;
    function Description: String; override;
  end;

var
  EditorsStylesDemoNoteBookFrame: TEditorsStylesDemoNoteBookFrame;

implementation

uses EditorsStylesDemoData;

{$R *.dfm}

{ TEditorsStylesDemoNoteBookFrame }

constructor TEditorsStylesDemoNoteBookFrame.Create(AOwner: TComponent);
var
  ABitmap: TBitmap;
begin
  inherited Create(AOwner);
  cbFontColor.Properties.PrepareDelphiColorList(False, False);
  HintStyle := hcstRoundedInfo;
  FDisplayStyle := shtWood;
  FTempDisplayStyle := shtWood;

  FSpiralImageControl := TSpiralImageControl.Create(Self);
  FSpiralImageControl.Parent := pnSpiral;
  FSpiralImageControl.Align := alClient;
  FSpiralImageControl.Bitmap.Width :=  pnSpiral.Width;
  FSpiralImageControl.Bitmap.Height :=  25;
  ABitmap := TBitmap.Create;
  try
    ABitmap.LoadFromFile(StylesIniPath + 'scWood.bmp');
    FSpiralImageControl.Bitmap.Canvas.StretchDraw(FSpiralImageControl.Bitmap.Canvas.ClipRect, ABitmap);
  finally
    ABitmap.Free;
  end;
  cxFontNameComboBox.EditValue := 'MS Sans Serif';
end;

procedure TEditorsStylesDemoNoteBookFrame.dsNoteBookDataChange(Sender: TObject;
  Field: TField);
begin
  with EditorsStylesDemoDataDM do
  begin
    if Field = tblNoteBookNoteFont then
      dbmText.Style.Font.Name := string(tblNoteBookNoteFont.Value) else
    if Field = tblNoteBookNoteFontColor then
      dbmText.Style.Font.Color := tblNoteBookNoteFontColor.Value else
    if Field = tblNoteBookNOTETEXTSIZE then
      dbmText.Style.Font.Size := tblNoteBookNOTETEXTSIZE.Value else
    if Field = tblNoteBookNOTETEXTBKCOLOR then
      dbmText.Style.Color := tblNoteBookNOTETEXTBKCOLOR.Value;
  end;
end;

procedure TEditorsStylesDemoNoteBookFrame.tblNoteBookAfterScroll(
  DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
  begin
    dbmText.Style.Font.Color := 0;
    dbmText.Style.Font.Name := 'MS Sans Serif';
  end
  else
    with EditorsStylesDemoDataDM do
    begin
      dbmText.Style.Font.Color := tblNoteBookNoteFontColor.Value;
      dbmText.Style.Font.Name := string(tblNoteBookNoteFont.Value);
      dbmText.Style.Font.Size := tblNoteBookNOTETEXTSIZE.Value;
      dbmText.Style.Color := tblNoteBookNOTETEXTBKCOLOR.Value;
    end;
end;

function TEditorsStylesDemoNoteBookFrame.Name: string;
begin
  Result := 'Notebook';
end;

function TEditorsStylesDemoNoteBookFrame.BriefName: string;
begin
  Result := 'Notebook';
end;

function TEditorsStylesDemoNoteBookFrame.StylesIniPath: string;
begin
  Result := 'StylesFrmNoteBook\';
end;

function TEditorsStylesDemoNoteBookFrame.Description: String;
begin
  Result := 'Notebook Notes';
end;

function TEditorsStylesDemoNoteBookFrame.ShowControlsAboveDescription: Boolean;
begin
  Result := True;
end;

{ TSpiralImageControl }

constructor TSpiralImageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitMap := TBitMap.Create;
end;

destructor TSpiralImageControl.Destroy;
begin
  FBitMap.Free;
  inherited;
end;

procedure TSpiralImageControl.Paint;
begin
  Canvas.FillRect(ClientRect, FBitMap);
end;

procedure TEditorsStylesDemoNoteBookFrame.ChangeDisplayStyle(
  ADisplayStyle: TcxStyleSheetType);
var
  sFileName: String;
  ABitmap: TBitmap;
begin
  inherited ChangeDisplayStyle(ADisplayStyle);
  case FTempDisplayStyle of
    shtLightBlue: sFileName := 'scLightBlue.bmp';
    shtLightGray: sFileName := 'scLightGray.bmp';
    shtWood: sFileName := 'scWood.bmp';
    shtRainyDay: sFileName := 'scRainyDay.bmp';
    shtBrick: sFileName := 'scBrick.bmp';
    shtDeepSea: sFileName := 'scDeepSea.bmp';
  end;
  ABitmap := TBitmap.Create;
  try
    ABitmap.LoadFromFile(StylesIniPath + sFileName);
    FSpiralImageControl.Bitmap.Canvas.StretchDraw(FSpiralImageControl.Bitmap.Canvas.ClipRect, ABitmap);
    FSpiralImageControl.Paint;
  finally
    ABitmap.Free;
  end;
end;

procedure TEditorsStylesDemoNoteBookFrame.cxSpinEditPropertiesChange(
  Sender: TObject);
begin
  memDescrip.Style.Font.Size := StrToInt(cxSpinEdit.Text);
end;

procedure TEditorsStylesDemoNoteBookFrame.cxFontNameComboBoxPropertiesChange(
  Sender: TObject);
begin
  memDescrip.Style.Font.Name := cxFontNameComboBox.EditValue;
end;

procedure TEditorsStylesDemoNoteBookFrame.FormShow(Sender: TObject);
begin
  EditorsStylesDemoDataDM.tblNoteBook.AfterScroll := tblNoteBookAfterScroll;
  EditorsStylesDemoDataDM.dsNoteBook.OnDataChange := dsNoteBookDataChange;
  tblNoteBookAfterScroll(EditorsStylesDemoDataDM.tblNoteBook);
end;

initialization
  EditorsStylesDemoFrameManager.RegisterFrameClass(TEditorsStylesDemoNoteBookFrame);

end.
