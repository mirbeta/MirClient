unit GridModeDemoData;

interface

uses
  Forms,
  SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxClasses,
  cxGridTableView, DBTables;

type
  TGridModeDemoDataDM = class(TDataModule)
    dsCars: TDataSource;
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    PaymentTypeImages: TImageList;
    qryHelper: TQuery;
    qryCars: TQuery;
    qryCustomers: TQuery;
    qryOrders: TQuery;
    UpdateSQLCars: TUpdateSQL;
    UpdateSQLOrders: TUpdateSQL;
    Database: TDatabase;
    procedure qryAfterDelete(DataSet: TDataSet);
    procedure qryAfterPost(DataSet: TDataSet);
    procedure qryBeforePost(DataSet: TDataSet);
    procedure qryCarsBeforeScroll(DataSet: TDataSet);
    procedure qryCarsAfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
    FPrevDataSetState: TDataSetState;
  public
    { Public declarations }
    function GetTableNameByDataSet(ADataSet: TDataSet): string;
  end;

var
  GridModeDemoDataDM: TGridModeDemoDataDM;

implementation

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  GridModeDemoMain;

{$R *.dfm}

procedure TGridModeDemoDataDM.qryAfterDelete(DataSet: TDataSet);
begin
  TQuery(DataSet).ApplyUpdates;
end;

procedure TGridModeDemoDataDM.qryAfterPost(DataSet: TDataSet);
var
  AKeyValue: Integer;
begin
  AKeyValue := -1;
  TQuery(DataSet).ApplyUpdates;
  if FPrevDataSetState = dsInsert then
    try
      DataSet.Close;
      qryHelper.SQL.Clear;
      qryHelper.SQL.Add('select MAX(ID) from ' +
        GetTableNameByDataSet(DataSet));
      qryHelper.Open;
      AKeyValue := qryHelper.Fields[0].AsInteger;
    finally
      qryHelper.Close;
      DataSet.Open;
      DataSet.Locate('ID',AKeyValue , []);
    end;
end;

procedure TGridModeDemoDataDM.qryBeforePost(DataSet: TDataSet);
begin
  FPrevDataSetState := DataSet.State;
end;

function TGridModeDemoDataDM.GetTableNameByDataSet(
  ADataSet: TDataSet): string;
begin
  Result := Copy(ADataSet.Name, 4, Length(ADataSet.Name)-3);
end;

procedure TGridModeDemoDataDM.qryCarsBeforeScroll(DataSet: TDataSet);
begin
  Screen.Cursor := crHourGlass;
end;

procedure TGridModeDemoDataDM.qryCarsAfterScroll(DataSet: TDataSet);
begin
  Screen.Cursor := crDefault;
end;

end.
