unit RealtorWorldLoanCalculator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DateUtils, Math,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxLabel, Menus, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, DB, cxDBData, cxCurrencyEdit, dxmdaset,
  cxGridChartView, cxGridDBChartView, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxGridCustomView, cxGrid,
  StdCtrls, cxButtons, cxDropDownEdit, cxTextEdit, cxMaskEdit, cxSpinEdit,
  cxSplitter, cxGroupBox, RealtorWorldBaseFrame, dxSkinsCore,
  dxSkinscxPCPainter, ExtCtrls;

const
  dxTermsOfLoan: array [0..6] of Word = (1, 5, 10, 15, 20, 25, 30);

type
  TfrmLoanCalculator = class(TfrmBase)
    mdMonthlyPayments: TdxMemData;
    mdMonthlyPaymentsMonth: TStringField;
    mdMonthlyPaymentsBalance: TFloatField;
    mdMonthlyPaymentsInterest: TFloatField;
    mdMonthlyPaymentsPrincipal: TFloatField;
    mdMonthlyPaymentsDate: TDateField;
    dtsMonthlyPayments: TDataSource;
    cxGroupBox1: TcxGroupBox;
    cxGrid2: TcxGrid;
    cxGrid2DBChartView1: TcxGridDBChartView;
    cxGrid2DBChartView1Series2: TcxGridDBChartSeries;
    cxGrid2DBChartView1Series1: TcxGridDBChartSeries;
    cxGrid2Level1: TcxGridLevel;
    mdYearlyPayments: TdxMemData;
    mdYearlyPaymentsYear: TIntegerField;
    FloatField2: TFloatField;
    FloatField3: TFloatField;
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
    lblMontlyPayment: TcxLabel;
    cxGroupBox6: TcxGroupBox;
    procedure btnCalculateClick(Sender: TObject);
    procedure cxSplitter1BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
  private
    FMinInterest: Real;
    FStepInterest: Real;
    FStartMonts: array of TDate;

    procedure CalculateYearlyPayments;
    procedure CalculateMonthlyPayments(ALoan, AInterest, AMontlyPayment: Real; AStartMonth: TDate; const AMonthsCount: Word);
    procedure InitializeInterests;
    procedure InitializeTerms;
    procedure InitializeStartMonths;
    function GetSelectedInterest: Real;
    function GetSelectedTerm: Word;
    function GetSelectedStartMonth: TDate;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeSourceData;
  end;

implementation

{$R *.dfm}

constructor TfrmLoanCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeSourceData;
  btnCalculateClick(nil);
end;

function GetAnuitentMontlyPayment(ALoan, AInterest: Real; AMonthsCount: Word): Real;
var
  ASpecifiedInterest: Real;
begin
  ASpecifiedInterest := AInterest / (100 * 12);
  Result := ALoan * ASpecifiedInterest / (1 - Power(1 + ASpecifiedInterest, -AMonthsCount));
end;

function GetMontlyInterestPayment(ALoanRest, AInterest: Real): Real;
begin
  Result := ALoanRest * AInterest/100 * 1/12;
end;

function GetMonthName(ADate: TDate): string;
begin
  Result := FormatDateTime('mmmm", "yyyy', ADate);
end;

procedure TfrmLoanCalculator.CalculateYearlyPayments;
var
  ABookmark: TBookmark;
  AYearPrev, AYear: Word;
begin
  Screen.Cursor := crHourGlass;
  mdMonthlyPayments.DisableControls;
  ABookmark := mdMonthlyPayments.GetBookmark;
  mdYearlyPayments.Close;
  mdYearlyPayments.Open;
  AYearPrev := 0;
  try
    mdMonthlyPayments.First;
    while not mdMonthlyPayments.Eof do
    begin
      AYear := YearOf(mdMonthlyPayments.FieldByName('Date').AsDateTime);
      if AYearPrev <> AYear then
      begin
        if dsInsert = mdYearlyPayments.State then
          mdYearlyPayments.Post;
        mdYearlyPayments.Append;
      end;
      mdYearlyPayments.FieldByName('Year').AsInteger := AYear;
      mdYearlyPayments.FieldByName('Interest').AsFloat := mdYearlyPayments.FieldByName('Interest').AsFloat + mdMonthlyPayments.FieldByName('Interest').AsFloat;
      mdYearlyPayments.FieldByName('Principal').AsFloat := mdYearlyPayments.FieldByName('Principal').AsFloat + mdMonthlyPayments.FieldByName('Principal').AsFloat;
      AYearPrev := AYear;
      mdMonthlyPayments.Next;
    end;
    if dsInsert = mdYearlyPayments.State then
      mdYearlyPayments.Post;
    mdMonthlyPayments.GotoBookmark(ABookmark);
  finally
    mdMonthlyPayments.FreeBookmark(ABookmark);
  end;
  mdMonthlyPayments.EnableControls;
  Screen.Cursor := crDefault;
end;

procedure TfrmLoanCalculator.CalculateMonthlyPayments(ALoan, AInterest, AMontlyPayment: Real; AStartMonth: TDate; const AMonthsCount: Word);
var
  ANum: Word;
  AInterestPayment: Real;
begin
  Screen.Cursor := crHourGlass;
  mdMonthlyPayments.DisableControls;
  mdMonthlyPayments.Close;
  mdMonthlyPayments.Open;
  for ANum := 1 to AMonthsCount do
  begin
    AInterestPayment := GetMontlyInterestPayment(ALoan, AInterest);
    if ANum = AMonthsCount then
      AMontlyPayment := ALoan + AInterestPayment;
    ALoan := ALoan + AInterestPayment - AMontlyPayment;
    mdMonthlyPayments.Append;
    mdMonthlyPayments.FieldByName('Date').AsDateTime := AStartMonth;
    mdMonthlyPayments.FieldByName('Month').AsString := IntToStr(ANum) + ' (' + GetMonthName(AStartMonth) + ')';
    mdMonthlyPayments.FieldByName('Balance').AsFloat := ALoan;
    mdMonthlyPayments.FieldByName('Interest').AsFloat := AInterestPayment;
    mdMonthlyPayments.FieldByName('Principal').AsFloat := AMontlyPayment - AInterestPayment;
    mdMonthlyPayments.Post;
    AStartMonth := IncMonth(AStartMonth);
  end;
  mdMonthlyPayments.First;
  mdMonthlyPayments.EnableControls;
  Screen.Cursor := crDefault;
end;

function TfrmLoanCalculator.GetSelectedInterest: Real;
begin
  Result := FMinInterest + cbInterests.ItemIndex * FStepInterest;
end;

procedure TfrmLoanCalculator.InitializeInterests;
const
  ACount = 100;
var
  I: Integer;
  AValue: Real;
begin
  FMinInterest := 2.5;
  FStepInterest := 0.125;
  cbInterests.Properties.Items.Clear;
  cbInterests.ItemIndex := -1;
  for I := 0 to ACount - 1 do
  begin
    AValue := FMinInterest + I * FStepInterest;
    cbInterests.Properties.Items.Add(Format('%.3f', [AValue])+'%');
    if Abs(AValue - 5.625) < 0.000001 then
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

procedure TfrmLoanCalculator.InitializeStartMonths;
var
  AStartMonth: TDate;
  AYear, AMonth, ADay: Word;
  I, ACountMonts: Integer;
begin
  DecodeDate(Date, AYear, AMonth, ADay);
  AStartMonth := IncMonth(RecodeDate(Date, AYear, AMonth, 1));
  AMonth := MonthOf(AStartMonth);
  ACountMonts := 13 - AMonth;
  FStartMonts := nil;
  SetLength(FStartMonts, ACountMonts);
  cbStartMonths.Properties.Items.Clear;
  cbStartMonths.ItemIndex := -1;
  for I := AMonth to 12 do
  begin
    FStartMonts[I - AMonth] := AStartMonth;
    cbStartMonths.Properties.Items.Add(GetMonthName(FStartMonts[I - AMonth]));
    AStartMonth := IncMonth(AStartMonth);
  end;
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
  Result := FStartMonts[cbStartMonths.ItemIndex];
end;

procedure TfrmLoanCalculator.btnCalculateClick(Sender: TObject);
var
  ALoan, AInterest, AMontlyPayment: Real;
  AMonthsCount: Word;
begin
  ALoan := seLoan.Value;
  AInterest := GetSelectedInterest;
  AMonthsCount := GetSelectedTerm * 12;
  AMontlyPayment := GetAnuitentMontlyPayment(ALoan, AInterest, AMonthsCount);
  lblMontlyPayment.Caption := CurrToStrF(AMontlyPayment, ffCurrency, 0);
  CalculateMonthlyPayments(ALoan, AInterest, AMontlyPayment, GetSelectedStartMonth, AMonthsCount);
  CalculateYearlyPayments;
end;

procedure TfrmLoanCalculator.cxSplitter1BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

initialization
  RegisterFrame(IDLoanCalculator, TfrmLoanCalculator);

end.
