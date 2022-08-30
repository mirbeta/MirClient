unit DBUriStreamProvider;

{$I cxVer.inc}

interface

uses
  Classes, DBClient, dxRichEdit.Utils.UriStreamService, MidasLib;

{$I cxVer.Inc}

type
{ TdxDBUriStreamProvider }

  TdxDBUriStreamProvider = class(TInterfacedObject, IdxUriStreamProvider)
  private
    FDataSet: TClientDataSet;
    FField: string;
    FKey: string;
    FPrefix: string;
    procedure LoadData(ADataset: TClientDataSet);
  protected
    function GetStream(const AUri: string): TStream;
    function IsAsyncLoadingSupported(const AUri: string): Boolean;
  public
    constructor Create(ADataSet: TClientDataSet; AKey, AField, APrefix: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DB;

{ TdxDBUriStreamProvider }

constructor TdxDBUriStreamProvider.Create(ADataSet: TClientDataSet; AKey, AField, APrefix: string);
begin
  inherited Create;
  FDataSet := TClientDataSet.Create(nil);
  LoadData(ADataSet);
  FField := AField;
  FKey := AKey;
  FPrefix := APrefix;
end;

destructor TdxDBUriStreamProvider.Destroy;
begin
  FreeAndNil(FDataSet);
  inherited Destroy;
end;

procedure TdxDBUriStreamProvider.LoadData(ADataset: TClientDataSet);
begin
  FDataSet.CloneCursor(ADataset, True);
end;

function TdxDBUriStreamProvider.GetStream(const AUri: string): TStream;
var
  Id: Integer;
  AField: TBlobField;
begin
  TMonitor.Enter(FDataSet);
  try
    Result := nil;
    if Pos(FPrefix, Trim(AUri)) = 1 then
    begin
      if not TryStrToInt(Copy(Trim(AUri), 9, Length(AUri)), Id) then
        Exit;
      if FDataSet.Locate(FKey, Id, []) then
      begin
        AField := FDataSet.FieldByName(FField) as TBlobField;
        Result := TMemoryStream.Create;
        AField.SaveToStream(Result);
        Result.Position := 0;
      end;
    end;
  finally
    TMonitor.Exit(FDataSet);
  end;
end;

function TdxDBUriStreamProvider.IsAsyncLoadingSupported(const AUri: string): Boolean;
begin
  Result := False;
end;

end.
