unit RealtorWorld.LoanCalculator;

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections;

type

  { TMonthlyPayment }

  TMonthlyPayment = class
  private
    FNum: Integer;
    FBalance: Double;
    FInterest: Double;
    FDate: TDateTime;
    FPrincipal: Double;
    function GetMonth: string;
  public
    constructor Create(ANum: Integer; ADate: TDateTime; ABalance, AInterest, APrincipal: Double);
    property Num: Integer read FNum;
    property Date: TDateTime read FDate write FDate;
    property Balance: Double read FBalance write FBalance;
    property Interest: Double read FInterest write FInterest;
    property Principal: Double read FPrincipal write FPrincipal;
    property Month: string read GetMonth;

    class function GetMonthName(ADate: TDate): string; static;
  end;

  { TYearlyPayment }

  TYearlyPayment = class
  private
    FYear: Integer;
    FPrincipal: Double;
    FInterest: Double;
  public
    property Year: Integer read FYear write FYear;
    property Interest: Double read FInterest write FInterest;
    property Principal: Double read FPrincipal write FPrincipal;
  end;

  { TLoanCalculator }

  TLoanCalculator = class
  private
    FMonthlyPayments: TObjectList<TMonthlyPayment>;
    FYearlyPayments: TObjectList<TYearlyPayment>;
    FLoan: Double;
    FInterest: Double;
    FMonthsCount: Word;
    FStartMonth: TDate;
    FMonthlyPayment: Double;
  protected
    procedure CalculateMonthlyPayments;
    procedure CalculateYearlyPayments;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalculatePayments(ALoan, AInterest: Double; AMonthsCount: Word; AStartMonth: TDate);

    property Loan: Double read FLoan;
    property Interest: Double read FInterest;
    property MonthsCount: Word read FMonthsCount;
    property StartMonth: TDate read FStartMonth;
    property MonthlyPayment: Double read FMonthlyPayment;
    property MonthlyPayments: TObjectList<TMonthlyPayment> read FMonthlyPayments;
    property YearlyPayments: TObjectList<TYearlyPayment> read FYearlyPayments;

    class function GetAnuitentMonthlyPayment(ALoan, AInterest: Double; AMonthsCount: Word): Double; static;
    class function GetMonthlyInterestPayment(ALoanRest, AInterest: Double): Double; static;
    class function GetStartMonths(ADate: TDate): TArray<TDate>;
  end;

implementation

uses
  Math, DateUtils;

{ TMonthlyPayment }

constructor TMonthlyPayment.Create(ANum: Integer; ADate: TDateTime; ABalance, AInterest, APrincipal: Double);
begin
  inherited Create;
  FNum := ANum;
  FDate := ADate;
  FBalance := ABalance;
  FInterest := AInterest;
  FPrincipal := APrincipal;
end;

function TMonthlyPayment.GetMonth: string;
begin
  Result := IntToStr(FNum) + ' (' + GetMonthName(FDate) + ')';
end;

class function TMonthlyPayment.GetMonthName(ADate: TDate): string;
begin
  Result := FormatDateTime('mmmm", "yyyy', ADate);
end;

{ TLoanCalculator }

constructor TLoanCalculator.Create;
begin
  inherited Create;
  FMonthlyPayments := TObjectList<TMonthlyPayment>.Create;
  FYearlyPayments := TObjectList<TYearlyPayment>.Create;
end;

destructor TLoanCalculator.Destroy;
begin
  FMonthlyPayments.Free;
  FYearlyPayments.Free;
  inherited Destroy;
end;

procedure TLoanCalculator.CalculateMonthlyPayments;
var
  ANum: Integer;
  AInterest: Double;
  AMonthly: Double;
  ALoan: Double;
  AStartMonth: TDate;
  AMonthlyPayment: TMonthlyPayment;
begin
  AMonthly := FMonthlyPayment;
  ALoan := FLoan;
  AStartMonth := FStartMonth;
  for ANum := 1 to FMonthsCount do
  begin
    AInterest := GetMonthlyInterestPayment(ALoan, FInterest);
    if ANum = FMonthsCount then
      AMonthly := ALoan + AInterest;
    ALoan := ALoan + AInterest - AMonthly;
    AMonthlyPayment := TMonthlyPayment.Create(ANum, AStartMonth, ALoan, AInterest, AMonthly - AInterest);
    FMonthlyPayments.Add(AMonthlyPayment);
    AStartMonth := IncMonth(AStartMonth);
  end;
end;

procedure TLoanCalculator.CalculatePayments(ALoan, AInterest: Double; AMonthsCount: Word; AStartMonth: TDate);
begin
  FLoan := ALoan;
  FInterest := AInterest;
  FMonthsCount := AMonthsCount;
  FStartMonth := AStartMonth;

  FMonthlyPayments.Clear;
  FYearlyPayments.Clear;

  FMonthlyPayment := GetAnuitentMonthlyPayment(ALoan, AInterest, AMonthsCount);
  CalculateMonthlyPayments;
  CalculateYearlyPayments;
end;

procedure TLoanCalculator.CalculateYearlyPayments;
var
  AYearPrev, AYear: Word;
  AMonthlyPayment: TMonthlyPayment;
  AYearlyPayment: TYearlyPayment;
begin
  AYearPrev := 0;
  AYearlyPayment := nil;
  for AMonthlyPayment in FMonthlyPayments do
  begin
    AYear := YearOf(AMonthlyPayment.Date);
    if AYearPrev <> AYear then
    begin
      if AYearlyPayment <> nil then
        FYearlyPayments.Add(AYearlyPayment);
      AYearlyPayment := TYearlyPayment.Create;
      AYearlyPayment.Year := AYear;
    end;
    AYearlyPayment.Interest := AYearlyPayment.Interest + AMonthlyPayment.Interest;
    AYearlyPayment.Principal := AYearlyPayment.Principal + AMonthlyPayment.Principal;
    AYearPrev := AYear;
  end;
  if AYearlyPayment <> nil then
    FYearlyPayments.Add(AYearlyPayment);
end;

class function TLoanCalculator.GetAnuitentMonthlyPayment(ALoan, AInterest: Double; AMonthsCount: Word): Double;
var
  ASpecifiedInterest: Double;
begin
  ASpecifiedInterest := AInterest / (100 * 12);
  Result := ALoan * ASpecifiedInterest / (1 - Power(1 + ASpecifiedInterest, -AMonthsCount));
end;

class function TLoanCalculator.GetMonthlyInterestPayment(ALoanRest, AInterest: Double): Double;
begin
  Result := ALoanRest * AInterest/100 * 1/12;
end;

class function TLoanCalculator.GetStartMonths(ADate: TDate): TArray<TDate>;
var
  AStartMonth: TDate;
  AYear, AMonth, ADay: Word;
  I, ACountMonts: Integer;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  AStartMonth := IncMonth(RecodeDate(Date, AYear, AMonth, 1));
  AMonth := MonthOf(AStartMonth);
  ACountMonts := 13 - AMonth;
  SetLength(Result, ACountMonts);
  for I := AMonth to 12 do
  begin
    Result[I - AMonth] := AStartMonth;
    AStartMonth := IncMonth(AStartMonth);
  end;
end;

end.
