unit RealtorWorldLoanCalculator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls, Forms,
  Dialogs, DateUtils, Math, DB, Menus, ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxLabel, cxStyles, cxCustomData,
  cxData, cxFilter, cxDataStorage, cxDBData, cxCurrencyEdit,
  cxGridChartView, cxGridDBChartView, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxGridCustomView, cxGrid,
  cxButtons, cxDropDownEdit, cxTextEdit, cxMaskEdit, cxSpinEdit,
  cxSplitter, cxGroupBox, cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog,
  RealtorWorld.LoanCalculator,
  dxEMF.DataSet;

const
  dxTermsOfLoan: array [0..6] of Word = (1, 5, 10, 15, 20, 25, 30);

type
  TfrmLoanCalculator = class(TForm)
    dtsMonthlyPayments: TDataSource;
    cxGroupBox1: TcxGroupBox;
    cxGrid2: TcxGrid;
    cxGrid2DBChartView1: TcxGridDBChartView;
    cxGrid2DBChartView1Series2: TcxGridDBChartSeries;
    cxGrid2DBChartView1Series1: TcxGridDBChartSeries;
    cxGrid2Level1: TcxGridLevel;
    dtsYearlyPayments: TDataSource;
    cxSplitter1: TcxSplitter;
    cxGroupBox2: TcxGroupBox;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1DBTableView1RecId: TcxGridDBColumn;
    cxGrid1DBTableView1Month: TcxGridDBColumn;
    cxGrid1DBTableView1Balance: TcxGridDBColumn;
    cxGrid1DBTableView1Interest: TcxGridDBColumn;
    cxGrid1DBTableView1Principal: TcxGridDBColumn;
    cxGrid1Level1: TcxGridLevel;
    cxGroupBox3: TcxGroupBox;
    cxGroupBox5: TcxGroupBox;
    cxGroupBox4: TcxGroupBox;
    cxLabel6: TcxLabel;
    seLoan: TcxSpinEdit;
    cbInterests: TcxComboBox;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cbTerms: TcxComboBox;
    cxLabel4: TcxLabel;
    cbStartMonths: TcxComboBox;
    btnCalculate: TcxButton;
    cxLabel5: TcxLabel;
    lblMonthlyPayment: TcxLabel;
    cxGroupBox6: TcxGroupBox;
    edsYearlyPayments: TdxEMFDataSet;
    edsMonthlyPayments: TdxEMFDataSet;
    lbDescription: TLabel;
    mmMain: TMainMenu;
    miAbout: TMenuItem;
    procedure btnCalculateClick(Sender: TObject);
    procedure cxSplitter1BeforeClose(Sender: TObject; var AllowClose: Boolean);
    procedure miAboutClick(Sender: TObject);
  private
    FLoanCalculator: TLoanCalculator;
    FMinInterest: Double;
    FStepInterest: Double;
    FStartMonths: TArray<TDate>;

    procedure InitializeInterests;
    procedure InitializeTerms;
    procedure InitializeStartMonths;
    function GetSelectedInterest: Double;
    function GetSelectedTerm: Word;
    function GetSelectedStartMonth: TDate;
  protected
    class procedure RegisterLoanCalculatorEntities; static;
    class procedure UnRegisterLoanCalculatorEntities; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeSourceData;
  end;

var
  frmLoanCalculator: TfrmLoanCalculator;

implementation

uses
  dxEMF.Metadata,
  AboutDemoForm;

{$R *.dfm}

constructor TfrmLoanCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RegisterLoanCalculatorEntities;
  InitializeSourceData;
  FLoanCalculator := TLoanCalculator.Create;
  btnCalculateClick(nil);
end;

destructor TfrmLoanCalculator.Destroy;
begin
  FreeAndNil(FLoanCalculator);
  inherited Destroy;
  TfrmLoanCalculator.UnRegisterLoanCalculatorEntities;
end;

class procedure TfrmLoanCalculator.RegisterLoanCalculatorEntities;
begin
  EntityManager.RegisterEntity(TMonthlyPayment).Automapping.Key('Num').
    RegisterProperty('Month').VirtualColumn;
  EntityManager.RegisterEntity(TYearlyPayment).Automapping.Key('Year');
end;

class procedure TfrmLoanCalculator.UnRegisterLoanCalculatorEntities;
begin
  EntityManager.UnRegisterEntities([TMonthlyPayment, TYearlyPayment]);
end;

function TfrmLoanCalculator.GetSelectedInterest: Double;
begin
  Result := FMinInterest + cbInterests.ItemIndex * FStepInterest;
end;

procedure TfrmLoanCalculator.InitializeInterests;
const
  ACount = 100;
var
  I: Integer;
  AValue: Double;
begin
  FMinInterest := 2.5;
  FStepInterest := 0.125;
  cbInterests.Properties.Items.Clear;
  cbInterests.ItemIndex := -1;
  for I := 0 to ACount - 1 do
  begin
    AValue := FMinInterest + I * FStepInterest;
    cbInterests.Properties.Items.Add(Format('%.3f%%', [AValue]));
    if SameValue(AValue, 5.625, 0.000001) then
      cbInterests.ItemIndex := cbInterests.Properties.Items.Count - 1;
  end;
  if cbInterests.ItemIndex = -1 then cbInterests.ItemIndex := 0;
end;

procedure TfrmLoanCalculator.InitializeTerms;
const
  AEnding: array[Boolean] of string = ('', 's');
var
  I: Integer;
  AValue: Word;
begin
  cbTerms.Properties.Items.Clear;
  cbTerms.ItemIndex := -1;
  for I := 0 to High(dxTermsOfLoan) do
  begin
    AValue := dxTermsOfLoan[I];
    cbTerms.Properties.Items.Add(IntToStr(AValue) + ' year' + AEnding[dxTermsOfLoan[I] > 1]);
    if AValue = 15 then
      cbTerms.ItemIndex := cbTerms.Properties.Items.Count - 1;
  end;
  if cbTerms.ItemIndex = -1 then cbTerms.ItemIndex := 0;
end;

procedure TfrmLoanCalculator.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfrmLoanCalculator.InitializeStartMonths;
var
  AStartMonth: TDate;
begin
  cbStartMonths.Properties.Items.Clear;
  cbStartMonths.ItemIndex := -1;
  FStartMonths := TLoanCalculator.GetStartMonths(Date);
  for AStartMonth in FStartMonths do
    cbStartMonths.Properties.Items.Add(TMonthlyPayment.GetMonthName(AStartMonth));
  cbStartMonths.ItemIndex := 0;
end;

procedure TfrmLoanCalculator.InitializeSourceData;
begin
  seLoan.Value := 250000;
  InitializeInterests;
  InitializeTerms;
  InitializeStartMonths;
end;

function TfrmLoanCalculator.GetSelectedTerm: Word;
begin
  Result := dxTermsOfLoan[cbTerms.ItemIndex];
end;

function TfrmLoanCalculator.GetSelectedStartMonth: TDate;
begin
  Result := FStartMonths[cbStartMonths.ItemIndex];
end;

procedure TfrmLoanCalculator.btnCalculateClick(Sender: TObject);
var
  ALoan, AInterest: Double;
  AMonthsCount: Word;
begin
  ALoan := seLoan.Value;
  AInterest := GetSelectedInterest;
  AMonthsCount := GetSelectedTerm * 12;

  Screen.Cursor := crHourGlass;
  edsMonthlyPayments.Close;
  edsYearlyPayments.Close;
  FLoanCalculator.CalculatePayments(ALoan, AInterest, AMonthsCount, GetSelectedStartMonth);

  edsMonthlyPayments.AssignData<TMonthlyPayment>(FLoanCalculator.MonthlyPayments.ToArray);
  edsYearlyPayments.AssignData<TYearlyPayment>(FLoanCalculator.YearlyPayments.ToArray);

  lblMonthlyPayment.Caption := CurrToStrF(FLoanCalculator.MonthlyPayment, ffCurrency, 0);
  edsMonthlyPayments.Open;
  edsYearlyPayments.Open;
  cxGrid1DBTableView1.ApplyBestFit();
  Screen.Cursor := crDefault;
end;

procedure TfrmLoanCalculator.cxSplitter1BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

end.
