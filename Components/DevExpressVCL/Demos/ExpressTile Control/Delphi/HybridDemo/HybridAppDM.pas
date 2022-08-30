unit HybridAppDM;

interface

uses
  SysUtils, Classes, ImgList, Controls, cxGraphics, DB, DBClient, Provider, cxStyles, cxClasses, dxmdaset, MidasLib,
  dxLayoutLookAndFeels, cxContainer, cxEdit, dxSkinMetropolisDark, HybridAppDataPath,
  cxImageList, cxLocalization, dxCore, cxImageComboBox,
  Generics.Defaults, Generics.Collections;

type
  TdxEmployeeStatus = (esSalaried, esCommission, esContract, esTerminated, esOnLeave);
  TdxPersonPrefix = (ppDr, ppMr, ppMs, ppMiss, ppMrs);
  TdxProductCategory = (pcAutomation, pcMonitors, pcProjectors, pcTelevisions, pcVideoPlayers);
  TdxTaskStatus = (tsNotStarted, tsCompleted, tsInProgress, tsNeedAssist, tsDeferred);
  TdxTaskPriority = (tpLow, tpNormal, tpHigh, tpUrgent);

  TDM = class(TDataModule, IdxLocalizerListener)
    cxImageList1: TcxImageList;
    ilButtons: TcxImageList;
    dsEmployees: TDataSource;
    dsTasks: TDataSource;
    dsEvaluations: TDataSource;
    ilPriority: TcxImageList;
    dsEmployeesHelper: TDataSource;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    stEmplCaption: TcxStyle;
    stEmplContent: TcxStyle;
    dsProduct: TDataSource;
    dsStatesSpr: TDataSource;
    dtsDepartmentSpr: TDataSource;
    clDepartmentSpr: TClientDataSet;
    mdPrefixSpr: TdxMemData;
    mdPrefixSprPrefix_ID: TIntegerField;
    mdPrefixSprPrefix_Name: TStringField;
    mdStatusSpr: TdxMemData;
    mdStatusSprStatus_ID: TIntegerField;
    mdStatusSprStatus_Name: TStringField;
    dsPrefixSpr: TDataSource;
    dsStatusSpr: TDataSource;
    dsTasksHelper: TDataSource;
    dsCustomers: TDataSource;
    dsOrders: TDataSource;
    dsCustomerStores: TDataSource;
    dsCustomersHelper: TDataSource;
    clTaskEmployes: TClientDataSet;
    dsTaskEmployes: TDataSource;
    clTaskEmployesEmployee_Id: TIntegerField;
    clTaskEmployesEmployeeTask_Id: TIntegerField;
    clTaskEmployesFullName: TStringField;
    clTaskEmployesDepartment: TIntegerField;
    clTaskEmployesStatus: TIntegerField;
    clTaskEmployesPersonalProfile: TWideStringField;
    clTaskEmployesTitle: TWideStringField;
    mdCategoriesSpr: TdxMemData;
    mdCategoriesSprID: TIntegerField;
    mdCategoriesSprCategory: TStringField;
    dsCategoriesSpr: TDataSource;
    dsYearSales: TDataSource;
    clMonthSales2015: TClientDataSet;
    clMonthSales2015ProductID: TIntegerField;
    clMonthSales2015Month: TIntegerField;
    clMonthSales2015Total: TCurrencyField;
    dsMonthSales2015: TDataSource;
    dsCustomerEmployees: TDataSource;
    dsQuotes: TDataSource;
    mdStatesSpr: TdxMemData;
    mdStatesSprID: TIntegerField;
    mdStatesSprShortName: TStringField;
    mdStatesSprLongName: TWideStringField;
    mdStatesSprFlag48px: TBlobField;
    mdStatesSprFlag24px: TBlobField;
    clCustomerSales2015: TClientDataSet;
    IntegerField30: TIntegerField;
    IntegerField31: TIntegerField;
    CurrencyField1: TCurrencyField;
    dsCustomerSales2015: TDataSource;
    clEmployeesHelper: TClientDataSet;
    pvdEmployeesHelper: TDataSetProvider;
    clEmployeesHelperId: TIntegerField;
    clEmployeesHelperDepartment: TIntegerField;
    clEmployeesHelperTitle: TWideStringField;
    clEmployeesHelperStatus: TIntegerField;
    clEmployeesHelperHireDate: TDateTimeField;
    clEmployeesHelperPersonalProfile: TWideStringField;
    clEmployeesHelperFirstName: TWideStringField;
    clEmployeesHelperLastName: TWideStringField;
    clEmployeesHelperFullName: TWideStringField;
    clEmployeesHelperPrefix: TIntegerField;
    clEmployeesHelperHomePhone: TWideStringField;
    clEmployeesHelperMobilePhone: TWideStringField;
    clEmployeesHelperEmail: TWideStringField;
    clEmployeesHelperSkype: TWideStringField;
    clEmployeesHelperBirthDate: TDateTimeField;
    clEmployeesHelperPictureId: TIntegerField;
    clEmployeesHelperAddress_Line: TWideStringField;
    clEmployeesHelperAddress_City: TWideStringField;
    clEmployeesHelperAddress_State: TIntegerField;
    clEmployeesHelperAddress_ZipCode: TWideStringField;
    clEmployeesHelperAddress_Latitude: TFloatField;
    clEmployeesHelperAddress_Longitude: TFloatField;
    clEmployeesHelperProbationReason_Id: TIntegerField;
    clEmployeesHelperPicture: TBlobField;
    clEmployees: TClientDataSet;
    clEmployeesId: TIntegerField;
    clEmployeesDepartment: TIntegerField;
    clEmployeesTitle: TWideStringField;
    clEmployeesStatus: TIntegerField;
    clEmployeesHireDate: TDateTimeField;
    clEmployeesPersonalProfile: TWideStringField;
    clEmployeesFirstName: TWideStringField;
    clEmployeesLastName: TWideStringField;
    clEmployeesFullName: TWideStringField;
    clEmployeesPrefix: TIntegerField;
    clEmployeesHomePhone: TWideStringField;
    clEmployeesMobilePhone: TWideStringField;
    clEmployeesEmail: TWideStringField;
    clEmployeesSkype: TWideStringField;
    clEmployeesBirthDate: TDateTimeField;
    clEmployeesPictureId: TIntegerField;
    clEmployeesAddress_Line: TWideStringField;
    clEmployeesAddress_City: TWideStringField;
    clEmployeesAddress_State: TIntegerField;
    clEmployeesAddress_ZipCode: TWideStringField;
    clEmployeesAddress_Latitude: TFloatField;
    clEmployeesAddress_Longitude: TFloatField;
    clEmployeesProbationReason_Id: TIntegerField;
    clEmployeesPicture: TBlobField;
    clEmployeesFull_Address: TStringField;
    clEmployeesHelperFull_Address: TStringField;
    clTasksHelper: TClientDataSet;
    pvdTasksHelper: TDataSetProvider;
    clTasks: TClientDataSet;
    clEvaluations: TClientDataSet;
    clEvaluationsId: TIntegerField;
    clEvaluationsCreatedById: TIntegerField;
    clEvaluationsCreatedOn: TDateTimeField;
    clEvaluationsEmployeeId: TIntegerField;
    clEvaluationsSubject: TWideStringField;
    clEvaluationsDetails: TWideStringField;
    clEvaluationsRating: TIntegerField;
    clEvaluationsNoteCalc: TStringField;
    clProductHelper: TClientDataSet;
    pvdProductHelper: TDataSetProvider;
    clProduct: TClientDataSet;
    clCustomersHelper: TClientDataSet;
    pvdCustomersHelper: TDataSetProvider;
    clCustomers: TClientDataSet;
    clCustomerEmployees: TClientDataSet;
    clCustomerStores: TClientDataSet;
    clCustomerStoresId: TIntegerField;
    clCustomerStoresCustomerId: TIntegerField;
    clCustomerStoresAddress_Line: TWideStringField;
    clCustomerStoresAddress_City: TWideStringField;
    clCustomerStoresAddress_State: TIntegerField;
    clCustomerStoresAddress_ZipCode: TWideStringField;
    clCustomerStoresAddress_Latitude: TFloatField;
    clCustomerStoresAddress_Longitude: TFloatField;
    clCustomerStoresPhone: TWideStringField;
    clCustomerStoresFax: TWideStringField;
    clCustomerStoresTotalEmployees: TIntegerField;
    clCustomerStoresSquereFootage: TIntegerField;
    clCustomerStoresAnnualSales: TLargeintField;
    clCustomerStoresCrestId: TIntegerField;
    clCustomerStoresLocation: TWideStringField;
    clCustomerStoresAddress_Full: TStringField;
    clOrders: TClientDataSet;
    clYearSales: TClientDataSet;
    clYearSalesYear: TIntegerField;
    clYearSalesCategory: TIntegerField;
    clYearSalesTotal: TCurrencyField;
    clYearSalesCategoryName: TStringField;
    clQuotes: TClientDataSet;
    clQuotesId: TIntegerField;
    clQuotesNumber: TWideStringField;
    clQuotesCustomerId: TIntegerField;
    clQuotesCustomerStoreId: TIntegerField;
    clQuotesEmployeeId: TIntegerField;
    clQuotesDate: TDateTimeField;
    clQuotesSubTotal: TLargeintField;
    clQuotesShippingAmount: TLargeintField;
    clQuotesTotal: TLargeintField;
    clQuotesOpportunity: TFloatField;
    clQuotesOfficeStateID: TIntegerField;
    clQuotesOfficeState: TStringField;
    clQuotesOfficeCity: TStringField;
    clQuotesPercent: TFloatField;
    mdTaskStatus: TdxMemData;
    mdTaskStatusID: TIntegerField;
    mdTaskStatusStatusName: TStringField;
    dsTaskStatus: TDataSource;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    ilCrests: TcxImageList;
    pvdCustomerStoresHelper: TDataSetProvider;
    clCustomerStoresHelper: TClientDataSet;
    dsCustomerStoresHelper: TDataSource;
    clCustomerStoresHelperId: TIntegerField;
    clCustomerStoresHelperCustomerId: TIntegerField;
    clCustomerStoresHelperAddress_Line: TWideStringField;
    clCustomerStoresHelperAddress_City: TWideStringField;
    clCustomerStoresHelperAddress_State: TIntegerField;
    clCustomerStoresHelperAddress_ZipCode: TWideStringField;
    clCustomerStoresHelperAddress_Latitude: TFloatField;
    clCustomerStoresHelperAddress_Longitude: TFloatField;
    clCustomerStoresHelperPhone: TWideStringField;
    clCustomerStoresHelperFax: TWideStringField;
    clCustomerStoresHelperTotalEmployees: TIntegerField;
    clCustomerStoresHelperSquereFootage: TIntegerField;
    clCustomerStoresHelperAnnualSales: TLargeintField;
    clCustomerStoresHelperCrestId: TIntegerField;
    clCustomerStoresHelperLocation: TWideStringField;
    clCustomerStoresHelperAddress_Full: TStringField;
    cxEditStyleController1: TcxEditStyleController;
    clOrdersRecId: TIntegerField;
    clOrdersId: TIntegerField;
    clOrdersInvoiceNumber: TWideStringField;
    clOrdersCustomerId: TIntegerField;
    clOrdersStoreId: TIntegerField;
    clOrdersEmployeeId: TIntegerField;
    clOrdersOrderDate: TDateTimeField;
    clOrdersPONumber: TWideStringField;
    clOrdersSaleAmount: TLargeintField;
    clOrdersShippingAmount: TLargeintField;
    clOrdersTotalAmount: TLargeintField;
    clOrdersShipDate: TDateTimeField;
    clOrdersShipMethod: TIntegerField;
    clOrdersOrderTerms: TWideStringField;
    clOrderItems: TClientDataSet;
    dsOrderItems: TDataSource;
    clOrderItemsRecId: TIntegerField;
    clOrderItemsId: TIntegerField;
    clOrderItemsOrderId: TIntegerField;
    clOrderItemsProductId: TIntegerField;
    clOrderItemsProductUnits: TIntegerField;
    clOrderItemsProductPrice: TLargeintField;
    clOrderItemsDiscount: TLargeintField;
    clOrderItemsTotal: TLargeintField;
    clOrderItemsCategory: TIntegerField;
    clOrderItemsOrderDate: TDateTimeField;
    clOrderItemsName: TStringField;
    clProductRecId: TIntegerField;
    clProductId: TIntegerField;
    clProductName: TWideStringField;
    clProductDescription: TWideStringField;
    clProductProductionStart: TDateTimeField;
    clProductAvailable: TBooleanField;
    clProductPDF: TBlobField;
    clProductSupportId: TIntegerField;
    clProductEngineerId: TIntegerField;
    clProductCurrentInventory: TIntegerField;
    clProductBackorder: TIntegerField;
    clProductManufacturing: TIntegerField;
    clProductBarcode: TBlobField;
    clProductPrimaryImageId: TIntegerField;
    clProductCost: TLargeintField;
    clProductSalePrice: TLargeintField;
    clProductRetailPrice: TLargeintField;
    clProductConsumerRating: TFloatField;
    clProductCategory: TIntegerField;
    clProductHelperRecId: TIntegerField;
    clProductHelperId: TIntegerField;
    clProductHelperName: TWideStringField;
    clProductHelperDescription: TWideStringField;
    clProductHelperProductionStart: TDateTimeField;
    clProductHelperAvailable: TBooleanField;
    clProductHelperPDF: TBlobField;
    clProductHelperSupportId: TIntegerField;
    clProductHelperEngineerId: TIntegerField;
    clProductHelperCurrentInventory: TIntegerField;
    clProductHelperBackorder: TIntegerField;
    clProductHelperManufacturing: TIntegerField;
    clProductHelperBarcode: TBlobField;
    clProductHelperPrimaryImageId: TIntegerField;
    clProductHelperCost: TLargeintField;
    clProductHelperSalePrice: TLargeintField;
    clProductHelperRetailPrice: TLargeintField;
    clProductHelperConsumerRating: TFloatField;
    clProductHelperCategory: TIntegerField;
    clProductTotal2015: TCurrencyField;
    clCustomersRecId: TIntegerField;
    clCustomersId: TIntegerField;
    clCustomersName: TWideStringField;
    clCustomersHomeOffice_Line: TWideStringField;
    clCustomersHomeOffice_City: TWideStringField;
    clCustomersHomeOffice_State: TIntegerField;
    clCustomersHomeOffice_ZipCode: TWideStringField;
    clCustomersHomeOffice_Latitude: TFloatField;
    clCustomersHomeOffice_Longitude: TFloatField;
    clCustomersBillingAddress_Line: TWideStringField;
    clCustomersBillingAddress_City: TWideStringField;
    clCustomersBillingAddress_State: TIntegerField;
    clCustomersBillingAddress_ZipCode: TWideStringField;
    clCustomersBillingAddress_Latitude: TFloatField;
    clCustomersBillingAddress_Longitude: TFloatField;
    clCustomersPhone: TWideStringField;
    clCustomersFax: TWideStringField;
    clCustomersWebsite: TWideStringField;
    clCustomersAnnualRevenue: TLargeintField;
    clCustomersTotalStores: TIntegerField;
    clCustomersTotalEmployees: TIntegerField;
    clCustomersStatus: TIntegerField;
    clCustomersProfile: TWideStringField;
    clCustomersLogo: TBlobField;
    clCustomersHelperRecId: TIntegerField;
    clCustomersHelperId: TIntegerField;
    clCustomersHelperName: TWideStringField;
    clCustomersHelperHomeOffice_Line: TWideStringField;
    clCustomersHelperHomeOffice_City: TWideStringField;
    clCustomersHelperHomeOffice_State: TIntegerField;
    clCustomersHelperHomeOffice_ZipCode: TWideStringField;
    clCustomersHelperHomeOffice_Latitude: TFloatField;
    clCustomersHelperHomeOffice_Longitude: TFloatField;
    clCustomersHelperBillingAddress_Line: TWideStringField;
    clCustomersHelperBillingAddress_City: TWideStringField;
    clCustomersHelperBillingAddress_State: TIntegerField;
    clCustomersHelperBillingAddress_ZipCode: TWideStringField;
    clCustomersHelperBillingAddress_Latitude: TFloatField;
    clCustomersHelperBillingAddress_Longitude: TFloatField;
    clCustomersHelperPhone: TWideStringField;
    clCustomersHelperFax: TWideStringField;
    clCustomersHelperWebsite: TWideStringField;
    clCustomersHelperAnnualRevenue: TLargeintField;
    clCustomersHelperTotalStores: TIntegerField;
    clCustomersHelperTotalEmployees: TIntegerField;
    clCustomersHelperStatus: TIntegerField;
    clCustomersHelperProfile: TWideStringField;
    clCustomersHelperLogo: TBlobField;
    clCustomersTotal2015: TCurrencyField;
    dxLayoutCxLookAndFeelMetropolisDark: TdxLayoutCxLookAndFeel;
    pvdCustomerEmployeesHelper: TDataSetProvider;
    clCustomerEmployeesHelper: TClientDataSet;
    dxLayoutCxLookAndFeelNavy: TdxLayoutCxLookAndFeel;
    dsEmployeesTasks: TDataSource;
    clEmployeesTasks: TClientDataSet;
    pvdEmployeesTasks: TDataSetProvider;
    cxLocalizer1: TcxLocalizer;
    procedure clTaskEmployesCalcFields(DataSet: TDataSet);
    procedure clEmployeesHelperCalcFields(DataSet: TDataSet);
    procedure clEmployeesAfterPost(DataSet: TDataSet);
    procedure clTasksAfterPost(DataSet: TDataSet);
    procedure clEvaluationsCalcFields(DataSet: TDataSet);
    procedure clCustomerStoresCalcFields(DataSet: TDataSet);
    procedure clOrderItemsCalcFields(DataSet: TDataSet);
    procedure clQuotesCalcFields(DataSet: TDataSet);
    procedure clProductCalcFields(DataSet: TDataSet);
    procedure clCustomersCalcFields(DataSet: TDataSet);
    procedure mdTaskStatusStatusNameGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure mdStatusSprStatus_NameGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure mdPrefixSprPrefix_NameGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
  private
    FCustomersFilterID: Integer;
    FCustomerEmployeesFilterID: Integer;
    FCustomerStoresFilterID: Integer;
    FEmployeesStatusFilterValue: Integer;
    FEvalutionFilterID: Integer;
    FProductFilterID: Integer;
    FTaskStatusFilterValue: Integer;

    FEmployeeStatusCount: TDictionary<Integer, Integer>;
    FProductCategoryCount: TDictionary<Integer, Integer>;
    FTaskStatusCount: TDictionary<Integer, Integer>;

    function GetStateShortName(AStateIndex: Integer): string;
    procedure ReloadHelper(AHelperDts, AMasterDts: TDataSet);
  protected
    procedure LoadData; virtual;
    procedure SetLookAndFeelNativeStyle(AEnabled: Boolean); virtual;

    procedure Translate;
    procedure TranslationChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyCustomersFilter(AFilterID: Integer);
    procedure ApplyCustomerEmployeesFilter(ACustomerID: Integer);
    procedure ApplyCustomerStoresFilter(ACustomerID: Integer);
    procedure ApplyEmployeesStatusFilter(AStatus: Integer);
    procedure ApplyEvalutionFilter(AEmployee_ID: Integer);
    procedure ApplyYearSalesFilter(AYear: Integer);
    procedure ApplyProductFilter(AFilterID: Integer);
    procedure ApplyQuotesFilter(const AYear: Integer);
    procedure ApplySalesFilter(const AYear: Integer);
    procedure ApplyTaskStatusFilter(AStatus: Integer);

    procedure RecalculateEmployeesStatusCount;
    procedure RecalculateProductCategoriesCount;
    procedure RecalculateTasksCount;

    procedure ReloadCustomersHelper;
    procedure ReloadCustomerEmployeesHelper;
    procedure ReloadCustomersStoresHelper;
    procedure ReloadEmployeesHelper;
    procedure ReloadEmployeeTasks(const AEmployee_ID: Integer);
    procedure ReloadProductHelper;
    procedure ReloadTasksHelper(AHelper: TClientDataSet);

    function GetEmployeeStatusName(AStatus: TdxEmployeeStatus): string;
    function GetPrefixName(APrefix: TdxPersonPrefix): string;
    function GetProductCategoryName(ACategory: TdxProductCategory): string;
    function GetTaskPriorityTypeName(AType: TdxTaskPriority): string;
    function GetTaskStatusName(AStatus: TdxTaskStatus): string;
    procedure CalculateCount(ADictionary: TDictionary<Integer, Integer>; ADataset: TDataset;
      const AFieldName: string; AKeyOffset: Integer = 0);
    procedure LocalizePriorities(AProperties: TcxImageComboBoxProperties);

    property EmployeeStatusCount: TDictionary<Integer, Integer> read FEmployeeStatusCount;
    property ProductCategoryCount: TDictionary<Integer, Integer> read FProductCategoryCount;
    property TaskStatusCount: TDictionary<Integer, Integer> read FTaskStatusCount;
  end;

const
  cFilterValueOffset = 100;
  cAllCountTag = cFilterValueOffset + 1000;
  cHighPriorityTaskCountTag = cFilterValueOffset + Integer(tpHigh);
  cUrgentPriorityTaskCountTag = cFilterValueOffset + Integer(tpUrgent);

var
  DM: TDM;

implementation

uses
  Math, Graphics, Forms, StdCtrls, ComCtrls, StrUtils, DateUtils, LocalizationStrs;

{$R *.dfm}

procedure TDM.clCustomersCalcFields(DataSet: TDataSet);
var
  AResult: Real;
  ACustomerId: Integer;
begin
  AResult := 0;
  ACustomerId := DataSet.FieldByName('Id').AsInteger;
  if clCustomerSales2015.Locate('CustomerId', ACustomerId, []) then
  while not clCustomerSales2015.Eof and (clCustomerSales2015.FieldByName('CustomerId').AsInteger = ACustomerId) do
  begin
    AResult := AResult + clCustomerSales2015.FieldByName('Total').AsFloat;
    clCustomerSales2015.Next;
  end;
  DataSet.FieldByName('Total2015').AsCurrency := AResult;
end;

procedure TDM.clCustomerStoresCalcFields(DataSet: TDataSet);
begin
  with DataSet do
    FieldByName('Address_Full').AsString := Format('%s, %s %s',
      [FieldByName('Address_City').AsString, GetStateShortName(FieldByName('Address_State').AsInteger),
       FieldByName('Address_ZipCode').AsString]);
end;

procedure TDM.clOrderItemsCalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('Year').AsInteger := YearOf(DataSet.FieldByName('OrderDate').AsDateTime);
end;

procedure TDM.clProductCalcFields(DataSet: TDataSet);
var
  AResult: Real;
  AProductId: Integer;
begin
  AResult := 0;
  AProductId := DataSet.FieldByName('Id').AsInteger;
  if clMonthSales2015.Locate('ProductId', AProductId, []) then
  while not clMonthSales2015.Eof and (clMonthSales2015.FieldByName('ProductId').AsInteger = AProductId) do
  begin
    AResult := AResult + clMonthSales2015.FieldByName('Total').AsFloat;
    clMonthSales2015.Next;
  end;
  DataSet.FieldByName('Total2015').AsCurrency := AResult;
end;

procedure TDM.clEmployeesHelperCalcFields(DataSet: TDataSet);
begin
  with DataSet do
    FieldByName('Full_Address').AsString := Format('%s, %s, %s, %s',
      [FieldByName('Address_Line').AsString, FieldByName('Address_City').AsString,
       GetStateShortName(FieldByName('Address_State').AsInteger), FieldByName('Address_ZipCode').AsString]);
end;

procedure TDM.clEvaluationsCalcFields(DataSet: TDataSet);
var
  ARichEdit: TRichEdit;
  AStream: TStringStream;
  AForm: TForm;
begin
  if not clEvaluations.Filtered then
    Exit;
  AForm := TForm.Create(nil);
  try
    ARichEdit := TRichEdit.Create(nil);
    try
      ARichEdit.Parent := AForm;
      ARichEdit.Font.Size := 12;
      ARichEdit.Font.Name := 'Segoe UI';
      ARichEdit.ScrollBars := ssHorizontal;
      ARichEdit.Lines.Add(clEvaluationsSubject.Value);
      ARichEdit.Lines.Add(clEvaluationsDetails.Value);
      ARichEdit.SelStart := 0;
      ARichEdit.SelLength := Length(ARichEdit.Lines[0]);
      ARichEdit.SelAttributes.Style := [fsBold];
//      ARichEdit.SelAttributes.Size := 14;
      AStream := TStringStream.Create;
      try
        ARichEdit.Lines.SaveToStream(AStream);
        AStream.Position := 0;
        clEvaluationsNoteCalc.AsString := AStream.DataString;
      finally
        AStream.Free;
      end;
    finally
      ARichEdit.Free;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TDM.clTaskEmployesCalcFields(DataSet: TDataSet);
begin
  if not clEmployeesHelper.Active then
    ReloadEmployeesHelper;
  with DataSet do
    if clEmployeesHelper.FindKey([FieldByName('Employee_Id').AsInteger]) then
    begin
      FieldByName('Department').AsInteger := clEmployeesHelper.FieldByName('Department').AsInteger;
      FieldByName('Status').AsInteger := clEmployeesHelper.FieldByName('Status').AsInteger;
    end;
end;

procedure TDM.clQuotesCalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('Percent').AsFloat := DataSet.FieldByName('Opportunity').AsFloat * 100;
end;

constructor TDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomersFilterID := -1;
  FEmployeesStatusFilterValue := -1;
  FEvalutionFilterID := -1;
  FProductFilterID := -1;
  FTaskStatusFilterValue := -1;
  SetLookAndFeelNativeStyle(False);
  FEmployeeStatusCount := TDictionary<Integer, Integer>.Create;
  FTaskStatusCount := TDictionary<Integer, Integer>.Create;
  FProductCategoryCount := TDictionary<Integer, Integer>.Create;
  LoadData;
end;

destructor TDM.Destroy;
begin
  FreeAndNil(FProductCategoryCount);
  FreeAndNil(FEmployeeStatusCount);
  FreeAndNil(FTaskStatusCount);
  inherited Destroy;
end;

procedure TDM.ApplyCustomersFilter(AFilterID: Integer);
begin
  if FCustomersFilterID = AFilterID then
    Exit;
  clCustomers.DisableControls;
  try
    clCustomers.Filtered := False;
    FCustomersFilterID := AFilterID;
    case FCustomersFilterID of
      0:
        clCustomers.Filter := 'HomeOffice_State = 0';
      1:
        clCustomers.Filter := 'HomeOffice_State = 46';
      2:
        clCustomers.Filter := 'TotalStores > 30';
    end;
    clCustomers.Filtered := FCustomersFilterID <> -1;
  finally
    clCustomers.EnableControls;
  end;
end;

procedure TDM.ApplyCustomerEmployeesFilter(ACustomerID: Integer);
begin
  if FCustomerEmployeesFilterID = ACustomerID then
    Exit;
  clCustomerEmployees.DisableControls;
  try
    FCustomerEmployeesFilterID := ACustomerID;
    clCustomerEmployees.Filter := Format('CustomerID = %d', [FCustomerEmployeesFilterID]);
    clCustomerEmployees.Filtered := True;
  finally
    clCustomerEmployees.EnableControls;
  end;
end;

procedure TDM.ApplyCustomerStoresFilter(ACustomerID: Integer);
begin
  if FCustomerStoresFilterID = ACustomerID then
    Exit;
  clCustomerStores.DisableControls;
  try
    FCustomerStoresFilterID := ACustomerID;
    clCustomerStores.Filter := Format('CustomerID = %d', [FCustomerStoresFilterID]);
    clCustomerStores.Filtered := True;
  finally
    clCustomerStores.EnableControls;
  end;
end;

procedure TDM.ApplyEmployeesStatusFilter(AStatus: Integer);
begin
  if FEmployeesStatusFilterValue = AStatus then
    Exit;
  clEmployees.DisableControls;
  try
    clEmployees.Filtered := False;
    FEmployeesStatusFilterValue := AStatus;

    if FEmployeesStatusFilterValue in [0..4] then
      clEmployees.Filter := Format('Status = %d', [FEmployeesStatusFilterValue])
    else
      if FEmployeesStatusFilterValue >= cFilterValueOffset then
        clEmployees.Filter := Format('Status = %d', [FEmployeesStatusFilterValue - cFilterValueOffset]);

    clEmployees.Filtered := (FEmployeesStatusFilterValue <> -1) and (FEmployeesStatusFilterValue <> cAllCountTag);
  finally
    clEmployees.EnableControls;
  end;
end;

procedure TDM.ApplyEvalutionFilter(AEmployee_ID: Integer);
begin
  if FEvalutionFilterID = AEmployee_ID then
    Exit;
  clEvaluations.DisableControls;
  try
    clEvaluations.Filtered := False;
    FEvalutionFilterID := AEmployee_ID;
    clEvaluations.Filter := Format('EmployeeId = %d', [FEvalutionFilterID]);
    clEvaluations.Filtered := FEvalutionFilterID <> -1;
  finally
    clEvaluations.EnableControls;
  end;
end;

procedure TDM.ApplyYearSalesFilter(AYear: Integer);
begin
  clYearSales.Filter := Format('Year = %d', [AYear]);
  clYearSales.Filtered := True;
end;

procedure TDM.ApplyProductFilter(AFilterID: Integer);
const
  AFilterSubStr: array[0..3] of string = ('HD Video Player', '50', '21', 'Remote');
begin
  if FProductFilterID = AFilterID then
    Exit;
  clProduct.DisableControls;
  try
    clProduct.Filtered := False;
    FProductFilterID := AFilterID;
    if InRange(FProductFilterID, 1000, 1003) then
      clProduct.Filter := 'Name like ''%' + AFilterSubStr[FProductFilterID - 1000] + '%'''
    else
      if InRange(FProductFilterID, Integer(pcAutomation), Integer(pcVideoPlayers)) then
        clProduct.Filter := Format('Category = %d', [FProductFilterID])
      else
        if FProductFilterID >= cFilterValueOffset then
          clProduct.Filter := Format('Category = %d', [FProductFilterID - cFilterValueOffset])
        else
          FProductFilterID := -1;
    clProduct.Filtered := (FProductFilterID <> -1) and (FProductFilterID <> cAllCountTag);
  finally
    clProduct.EnableControls;
  end;
end;

procedure TDM.ApplyQuotesFilter(const AYear: Integer);
var
  ADateBeg, ADateEnd: TDate;
begin
  ADateBeg := RecodeDate(Date, AYear, 1, 1);
  ADateEnd := RecodeDate(Date, AYear, 12, 31);
  clQuotes.Filter := Format('Date >= ''%s'' and Date <= ''%s''', [DateToStr(ADateBeg), DateToStr(ADateEnd)]);
  clQuotes.Filtered := True;
end;

procedure TDM.ApplySalesFilter(const AYear: Integer);
var
  ADateBeg, ADateEnd: TDate;
begin
  ADateBeg := RecodeDate(Date, AYear, 1, 1);
  ADateEnd := RecodeDate(Date, AYear, 12, 31);
  clOrders.Filter := Format('OrderDate >= ''%s'' and OrderDate <= ''%s''', [DateToStr(ADateBeg), DateToStr(ADateEnd)]);
  clOrders.Filtered := True;
end;

procedure TDM.ApplyTaskStatusFilter(AStatus: Integer);
begin
  if FTaskStatusFilterValue = AStatus then
    Exit;
  clTasks.DisableControls;
  try
    clTasks.Filtered := False;
    FTaskStatusFilterValue := AStatus;
    if FTaskStatusFilterValue in [0..4] then
      clTasks.Filter := Format('Status = %d', [FTaskStatusFilterValue])
    else
      if FTaskStatusFilterValue >= cFilterValueOffset then
        clTasks.Filter := Format('Priority = %d', [FTaskStatusFilterValue - cFilterValueOffset]);
    clTasks.Filtered := (AStatus <> -1) and (FTaskStatusFilterValue <> cAllCountTag);
  finally
    clTasks.EnableControls;
  end;
end;

procedure TDM.clEmployeesAfterPost(DataSet: TDataSet);
begin
  RecalculateEmployeesStatusCount;
end;

procedure TDM.clTasksAfterPost(DataSet: TDataSet);
begin
  RecalculateTasksCount;
end;

function TDM.GetStateShortName(AStateIndex: Integer): string;
begin
  Result := '';
  if mdStatesSpr.Locate('ID', AStateIndex, []) then
    Result := mdStatesSpr.FieldByName('ShortName').AsString;
end;

procedure TDM.LoadData;
var
  ADataPath: string;
begin
  ADataPath := ExtractFilePath(Application.ExeName) + GetDataPath;
  mdStatesSpr.LoadFromBinaryFile(ADataPath + 'States.dat');
  clDepartmentSpr.LoadFromFile(ADataPath + 'Departments.cds');
  mdCategoriesSpr.LoadFromBinaryFile(ADataPath + 'Categories.dat');
  mdPrefixSpr.LoadFromBinaryFile(ADataPath + 'Prefix.dat');
  mdStatusSpr.LoadFromBinaryFile(ADataPath + 'Status.dat');
  mdTaskStatus.LoadFromBinaryFile(ADataPath + 'TaskStatus.dat');

  clYearSales.LoadFromFile(ADataPath + 'YearSales.cds');
  clCustomerSales2015.LoadFromFile(ADataPath + 'CustomerSales2015.cds');
  clMonthSales2015.LoadFromFile(ADataPath + 'MonthlySales2015.cds');

  clEmployees.LoadFromFile(ADataPath + 'Employees.cds');
  ReloadEmployeesHelper;
  clTasks.LoadFromFile(ADataPath + 'Tasks.cds');
  ReloadTasksHelper(clTasksHelper);
  ReloadTasksHelper(clEmployeesTasks);
  clTaskEmployes.LoadFromFile(ADataPath + 'TaskEmployees.cds');
  clEvaluations.LoadFromFile(ADataPath + 'Evaluations.cds');
  clProduct.LoadFromFile(ADataPath + 'Products.cds');
  ReloadProductHelper;
  clCustomers.LoadFromFile(ADataPath + 'Customers.cds');
  ReloadCustomersHelper;
  clCustomerEmployees.LoadFromFile(ADataPath + 'CustomerEmployees.cds');
  ReloadCustomerEmployeesHelper;
  clCustomerStores.LoadFromFile(ADataPath + 'CustomerStores.cds');
  ReloadCustomersStoresHelper;
  clOrders.LoadFromFile(ADataPath + 'Orders.cds');
  clOrderItems.LoadFromFile(ADataPath + 'OrderItems.cds');
  clQuotes.LoadFromFile(ADataPath + 'Quotes.cds');

  cxLocalizer1.Active := True;
end;

procedure TDM.RecalculateEmployeesStatusCount;
var
  I: TdxEmployeeStatus;
begin
  FEmployeeStatusCount.Clear;
  for I :=  Low(TdxEmployeeStatus) to High(TdxEmployeeStatus) do
    FEmployeeStatusCount.Add(Integer(I), 0);
  ReloadEmployeesHelper;

  clEmployeesHelper.First;
  while not clEmployeesHelper.EOF do
  begin
    CalculateCount(FEmployeeStatusCount, clEmployeesHelper, 'Status');
    clEmployeesHelper.Next;
  end;
  FEmployeeStatusCount.AddOrSetValue(cAllCountTag, clEmployeesHelper.RecordCount);
end;

procedure TDM.RecalculateProductCategoriesCount;
var
  I: TdxProductCategory;
begin
  FProductCategoryCount.Clear;
  for I :=  Low(TdxProductCategory) to High(TdxProductCategory) do
    FProductCategoryCount.Add(Integer(I), 0);
  ReloadProductHelper;
  FProductCategoryCount.AddOrSetValue(cAllCountTag, clProductHelper.RecordCount);

  clProductHelper.First;
  while not clProductHelper.EOF do
  begin
    CalculateCount(FProductCategoryCount, clProductHelper, 'Category');
    clProductHelper.Next;
  end;
end;

procedure TDM.RecalculateTasksCount;
var
  AStatus: TdxTaskStatus;
begin
  FTaskStatusCount.Clear;
  for AStatus :=  Low(TdxTaskStatus) to High(TdxTaskStatus) do
    FTaskStatusCount.Add(Integer(AStatus), 0);

  ReloadTasksHelper(DM.clTasksHelper);

  FTaskStatusCount.AddOrSetValue(cAllCountTag, clTasksHelper.RecordCount);

  clTasksHelper.First;
  while not clTasksHelper.EOF do
  begin
    CalculateCount(FTaskStatusCount, clTasksHelper, 'Status');
    CalculateCount(FTaskStatusCount, clTasksHelper, 'Priority', cFilterValueOffset);
    clTasksHelper.Next;
  end;
end;

procedure TDM.ReloadHelper(AHelperDts, AMasterDts: TDataSet);
const
  AFieldID = 'ID';
var
  ID: Integer;
  AFiltered: Boolean;
begin
  with AMasterDts do
  begin
    ID := FieldByName(AFieldID).AsInteger;
    AFiltered := Filtered;
    DisableControls;
    try
      Filtered := False;
      AHelperDts.Close;
      AHelperDts.Open;
    finally
      Open;
      Filtered := AFiltered;
      Locate(AFieldID, ID, []);
      EnableControls;
    end;
  end;
end;

procedure TDM.ReloadCustomersHelper;
begin
  ReloadHelper(clCustomersHelper, clCustomers);
end;

procedure TDM.ReloadCustomerEmployeesHelper;
begin
  ReloadHelper(clCustomerEmployeesHelper, clCustomerEmployees);
end;

procedure TDM.ReloadCustomersStoresHelper;
begin
  ReloadHelper(clCustomerStoresHelper, clCustomerStores);
end;

procedure TDM.ReloadEmployeesHelper;
begin
  ReloadHelper(clEmployeesHelper, clEmployees);
end;

procedure TDM.ReloadEmployeeTasks(const AEmployee_ID: Integer);
begin
  clEmployeesTasks.DisableControls;
  try
    clEmployeesTasks.Filtered := False;
    ReloadTasksHelper(clEmployeesTasks);
    clEmployeesTasks.Filter := Format('AssignedEmployeeId = %d', [AEmployee_ID]);
    clEmployeesTasks.Filtered := True;
  finally
    clEmployeesTasks.EnableControls;
  end;
end;

procedure TDM.ReloadProductHelper;
begin
  ReloadHelper(clProductHelper, clProduct);
end;

procedure TDM.ReloadTasksHelper(AHelper: TClientDataSet);
begin
  ReloadHelper(AHelper, clTasks);
end;

function TDM.GetEmployeeStatusName(AStatus: TdxEmployeeStatus): string;
begin
  case AStatus of
    esSalaried:
      Result := cxGetResourceString(@sEmployeeStatusSalaried);
    esCommission:
      Result := cxGetResourceString(@sEmployeeStatusCommission);
    esContract:
      Result := cxGetResourceString(@sEmployeeStatusContract);
    esTerminated:
      Result := cxGetResourceString(@sEmployeeStatusTerminated);
    esOnLeave:
      Result := cxGetResourceString(@sEmployeeStatusOnLeave);
  else
    Result := '';
  end;
end;

function TDM.GetPrefixName(APrefix: TdxPersonPrefix): string;
begin
  case APrefix of
    ppDr:
      Result := cxGetResourceString(@sDr);
    ppMr:
      Result := cxGetResourceString(@sMr);
    ppMs:
      Result := cxGetResourceString(@sMs);
    ppMiss:
      Result := cxGetResourceString(@sMiss);
    ppMrs:
      Result := cxGetResourceString(@sMrs);
  else
    Result := '';
  end;
end;

function TDM.GetProductCategoryName(ACategory: TdxProductCategory): string;
begin
  case ACategory of
    pcTelevisions:
      Result := cxGetResourceString(@sTelevisions);
    pcMonitors:
      Result := cxGetResourceString(@sMonitors);
    pcVideoPlayers:
      Result := cxGetResourceString(@sVideoPlayers);
    pcProjectors:
      Result := cxGetResourceString(@sProjectors);
    pcAutomation:
      Result := cxGetResourceString(@sAutomation);
  else
    Result := '';
  end;
end;

function TDM.GetTaskPriorityTypeName(AType: TdxTaskPriority): string;
begin
  case AType of
    tpLow:
      Result := cxGetResourceString(@sPriorityLow);
    tpNormal:
      Result := cxGetResourceString(@sPriorityNormal);
    tpHigh:
      Result := cxGetResourceString(@sPriorityHigh);
    tpUrgent:
      Result := cxGetResourceString(@sPriorityUrgent);
  else
    Result := '';
  end;
end;

function TDM.GetTaskStatusName(AStatus: TdxTaskStatus): string;
begin
  case AStatus of
    tsNotStarted:
      Result := cxGetResourceString(@sTaskStatusNotStarted);
    tsCompleted:
      Result := cxGetResourceString(@sCompleted);
    tsInProgress:
      Result := cxGetResourceString(@sTaskStatusInProgress);
    tsNeedAssist:
      Result := cxGetResourceString(@sTaskStatusNeedAssistance);
    tsDeferred:
      Result := cxGetResourceString(@sTaskStatusDeffered)
  else
    Result := '';
  end;
end;

procedure TDM.CalculateCount(ADictionary: TDictionary<Integer, Integer>; ADataset: TDataset;
  const AFieldName: string; AKeyOffset: Integer = 0);
var
  AKey: Integer;
  ACount: Integer;
begin
  AKey := AKeyOffset + ADataset.FieldByName(AFieldName).AsInteger;
  if not ADictionary.TryGetValue(AKey, ACount) then
    ACount := 0;
  Inc(ACount);
  ADictionary.AddOrSetValue(AKey, ACount);
end;

procedure TDM.LocalizePriorities(AProperties: TcxImageComboBoxProperties);
var
  I: Integer;
begin
  for I := 0 to AProperties.Items.Count - 1 do
    AProperties.Items[I].Description := GetTaskPriorityTypeName(TdxTaskPriority(AProperties.Items[I].ImageIndex));
end;

procedure TDM.mdPrefixSprPrefix_NameGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  AText: string;
begin
  AText := GetPrefixName(TdxPersonPrefix(mdPrefixSprPrefix_ID.Value));
  if AText = '' then
    AText := Text;
  Text := AText;
end;

procedure TDM.mdStatusSprStatus_NameGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  AText: string;
begin
  AText := GetEmployeeStatusName(TdxEmployeeStatus(mdStatusSprStatus_ID.Value));
  if AText = '' then
    AText := Text;
  Text := AText;
end;

procedure TDM.mdTaskStatusStatusNameGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  AText: string;
begin
  AText := GetTaskStatusName(TdxTaskStatus(mdTaskStatusID.Value));
  if AText = '' then
    AText := Text;
  Text := AText;
end;

procedure TDM.SetLookAndFeelNativeStyle(AEnabled: Boolean);
begin
  dxLayoutCxLookAndFeel1.LookAndFeel.NativeStyle := AEnabled;
  dxLayoutCxLookAndFeelMetropolisDark.LookAndFeel.NativeStyle := AEnabled;
  dxLayoutCxLookAndFeelNavy.LookAndFeel.NativeStyle := AEnabled;
end;

procedure TDM.Translate;
begin
  DM.mdTaskStatus.Refresh;
  DM.mdStatusSpr.Refresh;
  DM.mdPrefixSpr.Refresh;
end;

procedure TDM.TranslationChanged;
begin
  Translate;
end;

end.
