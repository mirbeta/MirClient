unit IssueListForms;

interface

uses
  Forms, Classes, Controls,IssueListForm, SysUtils;

type
  TFormInfo = class
  private
    FCaption : string;
    FFormClass: TFormClass;
    FForm: TfrmBasic;
    FID: Integer;
  protected
  public
    constructor Create(ID: Integer; AClassType: TFormClass; ACaption: string);
    procedure CreateForm(AOwner: TComponent);
    procedure DestroyForm;
    procedure HideForm;
    procedure ShowForm(AParent: TWinControl);

    property Form: TfrmBasic read FForm;
    property FormClass: TFormClass read FFormClass;
    property Caption: string read FCaption;
    property ID: Integer read FID;
  end;

  TFormManager = class
  private
    FActiveFormInfo: TFormInfo;
    FFormInfoList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFormInfo;
  protected
    function GetFormInfoByID(AFormID: Integer): TFormInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterForm(AFormID: Integer; AClassType: TFormClass; ACaption: string);
    procedure ShowForm(AFormID: Integer; AParent: TWinControl);
    property FormInfoList: TList read FFormInfoList;
    property ActiveFormInfo: TFormInfo read FActiveFormInfo;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFormInfo read GetItem; default;
  end;

  function FormManager: TFormManager;

implementation

var
  FInstance : TFormManager;

function FormManager: TFormManager;
begin
  if not Assigned(FInstance) then FInstance := TFormManager.Create;
  Result := FInstance;
end;

{ TFormInfo }

constructor TFormInfo.Create(ID: Integer; AClassType: TFormClass;
  ACaption: string);
begin
  inherited Create;
  FID := ID;
  FCaption := ACaption;
  FFormClass := AClassType;
end;

procedure TFormInfo.CreateForm(AOwner: TComponent);
begin
  FForm := FFormClass.Create(AOwner);
  FForm.Caption := FCaption;
end;

procedure TFormInfo.DestroyForm;
begin
  FForm.Free;
  FForm := nil;
end;

procedure TFormInfo.HideForm;
begin
  FForm.Visible := False;
end;

procedure TFormInfo.ShowForm(AParent: TWinControl);
begin
  if not Assigned(FForm) then CreateForm(AParent);
  FForm.Parent := AParent;
  FForm.Visible := True;
end;

{ TFormManager }

constructor TFormManager.Create;
begin
  inherited Create;
  FFormInfoList := TList.Create;
end;

destructor TFormManager.Destroy;
var
  I: Integer;
begin
try
  for I := 0 to FormInfoList.Count - 1 do
    TFormInfo(FormInfoList[I]).Free;
  FFormInfoList.Free;
except
  raise Exception.Create('s');
end;
  inherited Destroy;
end;

function TFormManager.GetCount: Integer;
begin
  Result := 0;
  if FFormInfoList <> nil then
    Result := FFormInfoList.Count;
end;

function TFormManager.GetFormInfoByID(AFormID: Integer): TFormInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Items[I].ID = AFormID then Exit;
  end;
  Result := nil;

end;

function TFormManager.GetItem(Index: Integer): TFormInfo;
begin
  Result := TFormInfo(FFormInfoList[Index]);
end;

procedure TFormManager.RegisterForm(AFormID: Integer;
  AClassType: TFormClass; ACaption: string);
var
  AInfo: TFormInfo;
begin
  AInfo := TFormInfo.Create(AFormID, AClassType, ACaption);
  FormInfoList.Add(AInfo);
end;

procedure TFormManager.ShowForm(AFormID: Integer; AParent: TWinControl);
var
  AFormInfo: TFormInfo;
begin
  if Assigned(ActiveFormInfo) then
    if ActiveFormInfo.ID = AFormID then
      Exit
    else
    begin
      ActiveFormInfo.HideForm;
      FActiveFormInfo := nil;
    end;

  AFormInfo := GetFormInfoByID(AFormID);
  if AFormInfo <> nil then
  begin
    AFormInfo.ShowForm(AParent);
    FActiveFormInfo := AFormInfo;
  end;
end;

initialization

finalization
  FormManager.Free;

end.
