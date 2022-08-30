{********************************************************************}
{ TADVGRIDPRINTSETTINGSDIALOG component & property editor            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Original version written by Christopher Sansone, ScholarSoft       }
{                                                                    }
{ Copyright TMS Software                                             }
{ © 1998-2015                                                        }
{ Email : info@tmssoftware.com                                       }
{ Web : http://www.tmssoftware.com                                   }
{********************************************************************}

unit asgprint;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, AdvGrid, Printers, Mask, AdvSpin;

type
  TAdvGridPrintSettingsForm = class(TForm)
    PreviewPanel: TPanel;
    PageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    HeadersTabSheet: TTabSheet;
    ButtonPanel: TPanel;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    BordersGroupBox: TGroupBox;
    BorderLabel: TLabel;
    BorderComboBox: TComboBox;
    BorderStyleLabel: TLabel;
    BorderStyleComboBox: TComboBox;
    FontDialog: TFontDialog;
    PreviewPaintBox: TPaintBox;
    GeneralGroupBox: TGroupBox;
    FitToPageLabel: TLabel;
    FitToPageComboBox: TComboBox;
    AutoSizeCheckBox: TCheckBox;
    CenterCheckBox: TCheckBox;
    RepeatRowsCheckBox: TCheckBox;
    RepeatColumnsCheckBox: TCheckBox;
    DateTimeGroupBox: TGroupBox;
    DatePositionLabel: TLabel;
    TimePositionLabel: TLabel;
    DateFormatLabel: TLabel;
    DateFormatEdit: TEdit;
    DatePositionComboBox: TComboBox;
    TimePositionComboBox: TComboBox;
    TitleGroupBox: TGroupBox;
    TitleMemo: TMemo;
    TitleTextLabel: TLabel;
    TitlePositionLabel: TLabel;
    TitlePositionComboBox: TComboBox;
    PageNumbersGroupBox: TGroupBox;
    PagesPrefixEdit: TEdit;
    PagesPrefixLabel: TLabel;
    PagesSeparatorLabel: TLabel;
    PagesSuffixLabel: TLabel;
    PagesSeparatorEdit: TEdit;
    PagesSuffixEdit: TEdit;
    PagesPositionLabel: TLabel;
    PagesPositionComboBox: TComboBox;
    MarginsTabSheet: TTabSheet;
    MarginsGroupBox: TGroupBox;
    TopMarginLabel: TLabel;
    LeftMarginLabel: TLabel;
    TopMarginSpinEdit: TAdvSpinEdit;
    LeftMarginSpinEdit: TAdvSpinEdit;
    RightMarginLabel: TLabel;
    RightMarginSpinEdit: TAdvSpinEdit;
    BottomMarginSpinEdit: TAdvSpinEdit;
    BottomMarginLabel: TLabel;
    OrientationGroupBox: TGroupBox;
    OrientationLabel: TLabel;
    OrientationComboBox: TComboBox;
    SpacingGroupBox: TGroupBox;
    RowSpacingLabel: TLabel;
    ColumnSpacingLabel: TLabel;
    RowSpacingSpinEdit: TAdvSpinEdit;
    ColumnSpacingSpinEdit: TAdvSpinEdit;
    FontGroupBox: TGroupBox;
    HeaderFontButton: TSpeedButton;
    FooterFontButton: TSpeedButton;
    TableFontButton: TSpeedButton;
    TitleMarginLabel: TLabel;
    TitleMarginSpinEdit: TAdvSpinEdit;
    printgraphicscheckbox: TCheckBox;
    dim1: TLabel;
    dim2: TLabel;
    dim3: TLabel;
    dim4: TLabel;
    dim5: TLabel;
    dim6: TLabel;
    dim7: TLabel;
    dimr1: TRadioButton;
    dimr2: TRadioButton;
    TabSheet1: TTabSheet;
    FileGroupBox: TGroupBox;
    LoadSettings: TSpeedButton;
    SaveSettings: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    AutoSizeRowCheck: TCheckBox;
    SpeedButton1: TSpeedButton;
    DisplFont: TCheckBox;
    procedure UpdatePreviewer;
    procedure UpdateControls;
    procedure UpdateDimensions;
    procedure BorderStyleComboBoxChange(Sender: TObject);
    procedure RowSpacingSpinEditChange(Sender: TObject);
    procedure TableFontButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FooterFontButtonClick(Sender: TObject);
    procedure HeaderFontButtonClick(Sender: TObject);
    procedure BorderComboBoxChange(Sender: TObject);
    procedure DatePositionComboBoxChange(Sender: TObject);
    procedure TitlePositionComboBoxChange(Sender: TObject);
    procedure PagesPositionComboBoxChange(Sender: TObject);
    procedure PreviewPaintBoxPaint(Sender: TObject);
    procedure OrientationComboBoxChange(Sender: TObject);
    procedure dimr1Click(Sender: TObject);
    procedure dimr2Click(Sender: TObject);
    procedure LoadSettingsClick(Sender: TObject);
    procedure SaveSettingsClick(Sender: TObject);
    procedure AutoSizeRowCheckClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DisplFontClick(Sender: TObject);
  private
    procedure ClearPreviewer;
  public
    { Public declarations }
    Loading, PrintPreview,Updating: Boolean;
    IniFile: string;
    Grid: TAdvStringGrid;
    procedure HideDisabledTabSheets;
  end;

  TPrintSettingsOption = (psBorders, psGeneral, psFonts, psDateTime, psTitle,
                          psPages, psMargins, psSpacing, psOrientation, psSaveSettings);
  TPrintSettingsOptions = set of TPrintSettingsOption;

  TPrintDimensions = (pdmm,pdinch);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridPrintSettingsDialog = class(TCommonDialog)
  private
    FForm: TAdvGridPrintSettingsForm;
    FGrid: TAdvStringGrid;
    FOptions: TPrintSettingsOptions;
    FPrintPreview: Boolean;
    FPrintDimensions: TPrintDimensions;
    FExecuting:boolean;
    FIniFile: string;
    FCaption: string;
    FPrintPreviewWidth: integer;
    procedure SetPrintDimensions(const Value: TPrintDimensions);
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure SetPrintPreviewWidth(const Value: integer);
  protected
    procedure SetOptions(AOptions: TPrintSettingsOptions);
    procedure SetPrintPreview(Value: Boolean);
    procedure EnableGroupBox(AGroupBox: TGroupBox; Enable: Boolean);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property Form: TAdvGridPrintSettingsForm read FForm;
  published
    property Caption: string read FCaption write FCaption;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property Options: TPrintSettingsOptions read FOptions write SetOptions;
    property PrintPreview: Boolean read FPrintPreview write SetPrintPreview default true;
    property PrintPreviewWidth: integer read FPrintPreviewWidth write SetPrintPreviewWidth default 250;
    property PrintDimensions:TPrintDimensions read FPrintDimensions write SetPrintDimensions;
    property IniFile: string read FIniFile write FIniFile;
  end;

//  procedure AssignPrinterSettings(Source, Dest: TPrintSettings);


implementation

{$R *.DFM}

constructor TAdvGridPrintSettingsDialog.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  FGrid := nil;

  if not (csDesigning in ComponentState) then
    FForm := TAdvGridPrintSettingsForm.Create(Self)
  else
    FForm := TAdvGridPrintSettingsForm.Create(Application);

  FForm.Loading := False;

  FPrintPreviewWidth := 250;

  FOptions := [psBorders, psGeneral, psFonts, psDateTime, psTitle,
               psPages, psMargins, psSpacing, psOrientation, psSaveSettings];
  PrintPreview := True;
end;

destructor TAdvGridPrintSettingsDialog.Destroy;
begin
  Form.Free;
  inherited Destroy;
end;

function TAdvGridPrintSettingsDialog.Execute: Boolean;
var
  FCurrentPrintSettings: TPrintSettings;
begin
  if not Assigned(FGrid) then
  begin
    raise Exception.Create('The dialog does not have a grid component assigned.');
    Result := False;
    Exit;
  end;

  FExecuting := True;

  Form.PrintPreview := PrintPreview;
  Form.Grid := FGrid;
  Form.IniFile := IniFile;
  if FCaption <> '' then
    Form.Caption := FCaption;

  FCurrentPrintSettings := TPrintSettings.Create(FGrid);
  try
    FCurrentPrintSettings.Assign(FGrid.PrintSettings);
//    AssignPrinterSettings(FGrid.PrintSettings, FCurrentPrintSettings);
    Form.UpdateControls;
    Form.Width := 534 - 250 + FPrintPreviewWidth;

    Form.OKButton.Left := (Form.Width - 2 * Form.OKButton.Width - 15) div 2;
    Form.CancelButton.Left := Form.OKButton.Left + Form.OKButton.Width + 15;

    Result := Form.ShowModal = mrOK;
    If not Result then
      FGrid.PrintSettings.Assign(FCurrentPrintSettings);
//      AssignPrinterSettings(FCurrentPrintSettings, FGrid.PrintSettings);
  finally
    FGrid.FastPrint := False;
    FCurrentPrintSettings.Free;
  end;

  FExecuting:=false;
end;

procedure TAdvGridPrintSettingsDialog.SetOptions(AOptions: TPrintSettingsOptions);
begin
  FOptions := AOptions;

  If csDesigning in ComponentState then Exit;

  With FForm do begin
    EnableGroupBox(BordersGroupBox, psBorders in FOptions);
    EnableGroupBox(GeneralGroupBox, psGeneral in FOptions);
    EnableGroupBox(FontGroupBox, psFonts in FOptions);
    EnableGroupBox(DateTimeGroupBox, psDateTime in FOptions);
    EnableGroupBox(TitleGroupBox, psTitle in FOptions);
    EnableGroupBox(PageNumbersGroupbox, psPages in FOptions);
    EnableGroupBox(MarginsGroupBox, psMargins in FOptions);
    EnableGroupBox(SpacingGroupBox, psSpacing in FOptions);
    EnableGroupBox(OrientationGroupBox, psOrientation in FOptions);
    EnableGroupBox(FileGroupBox, psSaveSettings in FOptions);
    HideDisabledTabSheets;
  end;
end;

procedure TAdvGridPrintSettingsDialog.EnableGroupBox(AGroupBox: TGroupBox;
                                                     Enable: Boolean);
var
  i: Integer;
begin
  with AGroupBox do
  begin
    Enabled := Enable;
    For i := 0 to ControlCount - 1 do
      Controls[i].Enabled := Enable;
  end;
end;

procedure TAdvGridPrintSettingsDialog.SetPrintPreview(Value: Boolean);
begin
  FPrintPreview := Value;

  If (csDesigning in ComponentState) then Exit;

  with FForm do
  begin
    PreviewPanel.Visible := Value;

    if PreviewPanel.Visible then
      ClientWidth := 250 + PageControl.Width
    else
      ClientWidth := PageControl.Width;

    OKButton.Left := Round(Width / 2 - OKButton.Width - 8);
    CancelButton.Left := Round(Width / 2 + 8);
  end;
end;

procedure TAdvGridPrintSettingsDialog.SetPrintPreviewWidth(
  const Value: integer);
begin
  if (Value >= 0) and (Value <= 2560) then
    FPrintPreviewWidth := Value;
end;

procedure TAdvGridPrintSettingsDialog.SetPrintDimensions(
  const Value: TPrintDimensions);
begin
  FPrintDimensions := Value;
  if csDesigning in ComponentState then Exit;
  with FForm do
  begin
    if value = pdmm then
      dimr1.checked := True else dimr2.checked := True;
  end;
end;


procedure TAdvGridPrintSettingsDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
     FGrid := Nil;

  inherited;
end;

procedure TAdvGridPrintSettingsForm.UpdateControls;
begin
  Loading := True;
  try
    with Grid.PrintSettings do
    begin
      BorderComboBox.ItemIndex := Ord(Borders);
      BorderStyleComboBox.ItemIndex := Ord(BorderStyle);
      CenterCheckBox.Checked := Centered;
      FitToPageComboBox.ItemIndex := Ord(FitToPage);
      AutoSizeCheckBox.Checked := not NoAutoSize;
      AutoSizeRowCheck.Checked := not NoAutoSizeRow;
      RepeatRowsCheckBox.Checked := RepeatFixedRows;
      RepeatColumnsCheckBox.Checked := RepeatFixedCols;
      PrintGraphicsCheckBox.Checked := PrintGraphics;
      DateFormatEdit.Text := DateFormat;
      DatePositionComboBox.ItemIndex := Ord(Date);
      TimePositionComboBox.ItemIndex := Ord(Time);
      DisplFont.Checked := UseDisplayFont;

      //TitleLines will not work unless TitleText is empty, so this will
      //set TitleLines.Text to equal TitleText
      If TitleText <> '' then begin
        TitleLines.Text := TitleText;
        TitleText := '';
      end;
      TitleMemo.Lines.Assign(TitleLines);

      TitlePositionComboBox.ItemIndex := Ord(Title);
      PagesPrefixEdit.Text := PagePrefix;
      PagesSeparatorEdit.Text := PageNumSep;
      PagesSuffixEdit.Text := PageSuffix;
      PagesPositionComboBox.ItemIndex := Ord(PageNr);

      UpdateDimensions;
      
      OrientationComboBox.ItemIndex := Ord(Orientation);
    end;

    BorderComboBox.OnChange(Self);
    DatePositionComboBox.OnChange(Self);
    TitlePositionComboBox.OnChange(Self);
    PagesPositionComboBox.OnChange(Self);
  finally
    Loading := False;
  end;
end;

procedure TAdvGridPrintSettingsForm.UpdatePreviewer;
begin
  if Loading then
    Exit;
  if not Assigned(Grid) then
    Exit;

  Updating := True;

  with Grid.PrintSettings do
  begin
    Borders := TPrintBorders(BorderComboBox.ItemIndex);
    BorderStyle := TPenStyle(BorderStyleComboBox.ItemIndex);
    Centered := CenterCheckBox.Checked;
    FitToPage := TFitToPage(FitToPageComboBox.ItemIndex);
    NoAutoSize := not AutoSizeCheckBox.Checked;
    NoAutoSizeRow := not AutoSizeRowCheck.Checked;
    RepeatFixedRows := RepeatRowsCheckBox.Checked;
    RepeatFixedCols := RepeatColumnsCheckBox.Checked;
    PrintGraphics := PrintGraphicsCheckbox.Checked;
    DateFormat := DateFormatEdit.Text;
    Date := TPrintPosition(DatePositionComboBox.ItemIndex);
    Time := TPrintPosition(TimePositionComboBox.ItemIndex);
    TitleLines.Assign(TitleMemo.Lines);
    Title := TPrintPosition(TitlePositionComboBox.ItemIndex);
    PagePrefix := PagesPrefixEdit.Text;
    PageNumSep := PagesSeparatorEdit.Text;
    PageSuffix := PagesSuffixEdit.Text;
    PageNr := TPrintPosition(PagesPositionComboBox.ItemIndex);
    UseDisplayFont := DisplFont.Checked;
    
    if dimr1.checked then
     begin
      RowSpacing := round(RowSpacingSpinEdit.FloatValue*10);
      ColumnSpacing := round(ColumnSpacingSpinEdit.FloatValue*10);
      HeaderSize := round(TopMarginSpinEdit.FloatValue*10);
      LeftSize := round(LeftMarginSpinEdit.FloatValue*10);
      FooterSize := round(BottomMarginSpinEdit.FloatValue*10);
      RightSize := round(RightMarginSpinEdit.FloatValue*10);
      TitleSpacing := round(TitleMarginSpinEdit.FloatValue*10);
     end
    else
     begin
      RowSpacing := round(RowSpacingSpinEdit.FloatValue*254);
      ColumnSpacing := round(ColumnSpacingSpinEdit.FloatValue*254);
      HeaderSize := round(TopMarginSpinEdit.FloatValue*254);
      LeftSize := round(LeftMarginSpinEdit.FloatValue*254);
      FooterSize := round(BottomMarginSpinEdit.FloatValue*254);
      RightSize := round(RightMarginSpinEdit.FloatValue*254);
      TitleSpacing := round(TitleMarginSpinEdit.FloatValue*254);
     end;

    Orientation := TPrinterOrientation(OrientationComboBox.ItemIndex);

  end;

  Updating := False;

  if PrintPreview then
  begin
    ClearPreviewer;
    Grid.PrintPreview(PreviewPaintBox.Canvas, PreviewPaintBox.ClientRect);
  end;
end;

procedure TAdvGridPrintSettingsForm.BorderStyleComboBoxChange(
  Sender: TObject);
begin
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.RowSpacingSpinEditChange(
  Sender: TObject);
begin
  if Updating then exit;
  
  with TAdvSpinEdit(Sender) do
  begin
    if (Text<>'') then
      if (FloatValue < MaxValue) then UpdatePreviewer;
  end;
end;

procedure TAdvGridPrintSettingsForm.TableFontButtonClick(Sender: TObject);
begin
  with FontDialog do
  begin
    Font.Assign(Grid.PrintSettings.Font);
    If Execute then
    begin
      Grid.PrintSettings.Font.Assign(Font);
      UpdatePreviewer;
    end;
  end;
end;

procedure TAdvGridPrintSettingsForm.ClearPreviewer;
begin
  with PreviewPaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClientRect);
  end;
end;

procedure TAdvGridPrintSettingsForm.HideDisabledTabSheets;
var
  i, j: Integer;
begin
  for i := 1 to PageControl.PageCount - 1 do
  with PageControl.Pages[i] do
  begin
    TabVisible := False;
    for j := 0 to ControlCount - 1 do
      if (Controls[j] is TGroupBox) and
         (TGroupBox(Controls[j]).Enabled) then
      begin
        TabVisible := True;
        Break;
      end;
  end;
end;

procedure TAdvGridPrintSettingsForm.FormShow(Sender: TObject);
begin
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.FooterFontButtonClick(Sender: TObject);
begin
  with FontDialog do
  begin
    Font.Assign(Grid.PrintSettings.FooterFont);
    if Execute then
    begin
      Grid.PrintSettings.FooterFont.Assign(Font);
      UpdatePreviewer;
    end;
  end;
end;

procedure TAdvGridPrintSettingsForm.HeaderFontButtonClick(Sender: TObject);
begin
  with FontDialog do
  begin
    Font.Assign(Grid.PrintSettings.HeaderFont);
    If Execute then
    begin
      Grid.PrintSettings.HeaderFont.Assign(Font);
      UpdatePreviewer;
    end;
  end;
end;

procedure TAdvGridPrintSettingsForm.BorderComboBoxChange(Sender: TObject);
begin
  BorderStyleComboBox.Enabled := BorderComboBox.ItemIndex > 0;
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.DatePositionComboBoxChange(
  Sender: TObject);
begin
  DateFormatEdit.Enabled := DatePositionComboBox.ItemIndex > 0;
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.TitlePositionComboBoxChange(
  Sender: TObject);
begin
  TitleMemo.Enabled := TitlePositionComboBox.ItemIndex > 0;
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.OrientationComboBoxChange(
  Sender: TObject);
begin
  UpdatePreviewer;
end;


procedure TAdvGridPrintSettingsForm.PagesPositionComboBoxChange(
  Sender: TObject);
begin
  with PagesPrefixEdit do
  begin
    Enabled := PagesPositionComboBox.ItemIndex > 0;
    PagesSeparatorEdit.Enabled := Enabled;
    PagesSuffixEdit.Enabled := Enabled;
  end;
  UpdatePreviewer; 
end;

{
procedure AssignPrinterSettings(Source, Dest: TPrintSettings);
begin
  with Dest do
  begin
    Borders := Source.Borders;
    BorderStyle := Source.BorderStyle;
    Centered := Source.Centered;
    ColumnSpacing := Source.ColumnSpacing;
    Date := Source.Date;
    DateFormat := Source.DateFormat;
    FitToPage := Source.FitToPage;
    FixedHeight := Source.FixedHeight;
    FixedWidth := Source.FixedWidth;
    Font.Assign(Source.Font);
    FooterFont.Assign(Source.FooterFont);
    FooterSize := Source.FooterSize;
    HeaderFont.Assign(Source.HeaderFont);
    HeaderSize := Source.HeaderSize;
    JobName := Source.JobName;
    LeftSize := Source.LeftSize;
    NoAutoSize := Source.NoAutoSize;
    NoAutoSizeRow := Source.NoAutoSizeRow;
    Orientation := Source.Orientation;

    PageNr := Source.PageNr;
    PageNumSep := Source.PageNumSep;
    PagePrefix := Source.PagePrefix;
    PageSuffix := Source.PageSuffix;
    PrintGraphics := Source.PrintGraphics;
    RepeatFixedCols := Source.RepeatFixedCols;
    RepeatFixedRows := Source.RepeatFixedRows;
    RightSize := Source.RightSize;
    RowSpacing := Source.RowSpacing;
    Time := Source.Time;
    Title := Source.Title;
    TitleLines.Assign(Source.TitleLines);
    TitleSpacing := Source.TitleSpacing;
    TitleText := Source.TitleText;
    UseFixedHeight := Source.UseFixedHeight;
    UseFixedWidth := Source.UseFixedWidth;
  end;
end;
}

procedure TAdvGridPrintSettingsForm.PreviewPaintBoxPaint(Sender: TObject);
begin
  If Loading then Exit;
  If not Assigned(Grid) then Exit;
  ClearPreviewer;
  Grid.FastPrint := True;
  Grid.PreviewPage := 1;
  Grid.PrintPreview(PreviewPaintBox.Canvas, PreviewPaintBox.ClientRect);
end;

procedure TAdvGridPrintSettingsForm.dimr1Click(Sender: TObject);
begin
  UpdateDimensions;
  dim1.caption:='mm';
  dim2.caption:='mm';
  dim3.caption:='mm';
  dim4.caption:='mm';
  dim5.caption:='mm';
  dim6.caption:='mm';
  dim7.caption:='mm';
end;

procedure TAdvGridPrintSettingsForm.dimr2Click(Sender: TObject);
begin
  UpdateDimensions;
  dim1.caption:='inch';
  dim2.caption:='inch';
  dim3.caption:='inch';
  dim4.caption:='inch';
  dim5.caption:='inch';
  dim6.caption:='inch';
  dim7.caption:='inch';
end;

procedure TAdvGridPrintSettingsForm.UpdateDimensions;
begin
  if not assigned(Grid) then Exit;
  Updating:=true;
  with Grid.PrintSettings do
  if dimr1.checked then
  begin
    RowSpacingSpinEdit.FloatValue := RowSpacing/10;
    ColumnSpacingSpinEdit.FloatValue := ColumnSpacing/10;
    TopMarginSpinEdit.FloatValue := HeaderSize/10;
    LeftMarginSpinEdit.FloatValue := LeftSize/10;
    BottomMarginSpinEdit.FloatValue := FooterSize/10;
    RightMarginSpinEdit.FloatValue := RightSize/10;
    TitleMarginSpinEdit.FloatValue := TitleSpacing/10;
  end
  else
  begin
    RowSpacingSpinEdit.FloatValue := RowSpacing/254;
    ColumnSpacingSpinEdit.FloatValue := ColumnSpacing/254;
    TopMarginSpinEdit.FloatValue := HeaderSize/254;
    LeftMarginSpinEdit.FloatValue := LeftSize/254;
    BottomMarginSpinEdit.FloatValue := FooterSize/254;
    RightMarginSpinEdit.FloatValue := RightSize/254;
    TitleMarginSpinEdit.FloatValue := TitleSpacing/254;
  end;
  Updating := false;
end;


procedure TAdvGridPrintSettingsForm.LoadSettingsClick(Sender: TObject);
var
  fn:string;
begin
  if IniFile = '' then
  begin
    if not OpenDialog1.Execute then Exit;
    fn := OpenDialog1.FileName;
  end
  else
    fn := IniFile;

  if (pos('.',fn) = 0) and (OpenDialog1.FilterIndex = 1) then
    fn := fn + '.ini';
  Grid.LoadPrintSettings(fn,'PRINTSETTINGS');
  UpdateControls;
  UpdatePreviewer;
  PreviewPaintBox.Invalidate;
end;

procedure TAdvGridPrintSettingsForm.SaveSettingsClick(Sender: TObject);
var
  fn:string;
begin
  if IniFile = '' then
  begin
    if not SaveDialog1.Execute then Exit;
    fn := SaveDialog1.FileName;
  end
  else
    fn := IniFile;

  if (Pos('.',fn) = 0) and (SaveDialog1.FilterIndex = 1) then
    fn := fn + '.ini';
  Grid.SavePrintSettings(fn,'PRINTSETTINGS');
end;

procedure TAdvGridPrintSettingsDialog.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
end;


procedure TAdvGridPrintSettingsForm.AutoSizeRowCheckClick(Sender: TObject);
begin
  UpdatePreviewer;
end;

procedure TAdvGridPrintSettingsForm.SpeedButton1Click(Sender: TObject);
begin
  with FontDialog do
  begin
    Font.Assign(Grid.PrintSettings.FixedFont);
    If Execute then
    begin
      Grid.PrintSettings.FixedFont.Assign(Font);
      UpdatePreviewer;
    end;
  end;
end;

procedure TAdvGridPrintSettingsForm.DisplFontClick(Sender: TObject);
begin
  UpdatePreviewer;
end;

end.
