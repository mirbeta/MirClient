{$I TMSDEFS.INC}
{***********************************************************************}
{ TDBPlannerMonthView component                                         }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2004 - 2012                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit DBPlannerMonthView;

interface

uses
  Classes, DB, SysUtils, PlanRecurr
  , PlannerMonthView, Planner, DBPlanner, Dialogs
  , Windows, ComObj, ActiveX, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TDBPlannerMonthView = class;

  TPlannerDataLink = class(TDataLink)
  private
    FDBPlannerMonthView: TDBPlannerMonthView;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADBPlannerMonthView: TDBPlannerMonthView);
    destructor Destroy; override;
  end;


  TPlannerDataBinding = class(TPersistent)
  private
    FPlannerMonthView: TDBPlannerMonthView;
    FAutoIncKey: Boolean;
    FSubjectField: string;
    FStartTimeField: string;
    FNotesField: string;
    FKeyField: string;
    FEndTimeField: string;
    FUpdateByQuery: Boolean;
    FRecurrencyField: string;
    FMinTimeField: string;
    FMaxTimeField: string;
    procedure SetEndTimeField(const Value: string);
    procedure SetKeyField(const Value: string);
    procedure SetNotesField(const Value: string);
    procedure SetStartTimeField(const Value: string);
    procedure SetSubjectField(const Value: string);
    procedure SetMaxTimeField(const Value: string);
    procedure SetMinTimeField(const Value: string);
    procedure SetRecurrencyField(const Value: string);
  public
    constructor Create(AOwner: TDBPlannerMonthView);
    procedure Assign(Source: TPersistent); override;
    property Owner: TDBPlannerMonthView read FPlannerMonthView;
  published
    property AutoIncKey: Boolean read FAutoIncKey write FAutoIncKey default False;
    property StartTimeField: string read FStartTimeField write SetStartTimeField;
    property EndTimeField: string read FEndTimeField write SetEndTimeField;
    property KeyField: string  read FKeyField write SetKeyField;
    property MinTimeField: string read FMinTimeField write SetMinTimeField;
    property MaxTimeField: string read FMaxTimeField write SetMaxTimeField;
    property NotesField: string read FNotesField write SetNotesField;
    property RecurrencyField: string read FRecurrencyField write SetRecurrencyField;
    property SubjectField: string read FSubjectField write SetSubjectField;
    property UpdateByQuery: Boolean read FUpdateByQuery write FUpdateByQuery default False;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBPlannerMonthView = class(TPlannerMonthView)
  private
    FDataLink: TPlannerDataLink;
    FIsUpdating: Boolean;
    FDataSource: TDataSource;
    FOnFieldsToItem: TFieldsItemEvent;
    FOnItemToFields: TFieldsItemEvent;
    FDataBinding: TPlannerDataBinding;
    FOnFieldsToTime: TFieldsToTimeEvent;
    FOnTimeToFields: TTimeToFieldsEvent;
    FOnCreateKey: TCreateKeyEvent;
    FOnInsertItem: TDBItemSQLEvent;
    FOnDeleteItem: TDBItemSQLEvent;
    FOnUpdateItem: TDBItemSQLEvent;
    FRecurrencyHandler: TRecurrencyHandler;    
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDataBinding(const Value: TPlannerDataBinding);
  protected
    procedure SetDateProc(const Value: TDatetime); override;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure FieldsToTime(Fields:TFields; var dtS: TDateTime; var dtE: TDateTime); virtual;
    procedure TimeToFields(Fields:TFields; dtS: TDateTime; dtE: TDateTime); virtual;

    procedure DoChangeMonth(dt1, dt2: TDateTime); override;
    procedure DoChangeYear(dt1, dt2: TDateTime); override;

    procedure ItemChanged(DBKey: string);

    procedure ItemSelected(Item: TPlannerItem); override;
    procedure ItemMoved(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate,ToEndDate: TDateTime); override;
    procedure ItemSized(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate,ToEndDate: TDateTime); override;
    procedure ItemEdited(Sender: TObject; Item: TPlannerItem); override;
    procedure ClearDBKey(Key: string);
    function CheckDataSet: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function CreateItemAtSelection: TPlannerItem; override;
    function CreateNonDBItem: TPlannerItem; 
    function CreateItem: TPlannerItem; override;
    procedure FreeItem(APlannerItem: TPlannerItem); override;
    procedure UpdateItem(APlannerItem: TPlannerItem); override;
    procedure RefreshItem(APlannerItem: TPlannerItem); virtual;    

    property IsUpdating: Boolean read FIsUpdating;
    procedure ClearDBItems; virtual;
    procedure ReadDBItems(UpdateKey: string); virtual;
    procedure ReadDBItem; virtual;
    procedure WriteDBItem; virtual;
    procedure AddDBItem; virtual;
    procedure DeleteDBItem; virtual;
    procedure SelectDBItem; virtual;
    procedure Refresh;
    function CreateKey: string; virtual;
  published
    property DataBinding: TPlannerDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read FDataSource write SetDataSource;

    property OnFieldsToItem: TFieldsItemEvent read FOnFieldsToItem write FOnFieldsToItem;
    property OnItemToFields: TFieldsItemEvent read FOnItemToFields write FOnItemToFields;
    property OnFieldsToTime: TFieldsToTimeEvent read FOnFieldsToTime write FOnFieldsToTime;
    property OnTimeToFields: TTimeToFieldsEvent read FOnTimeToFields write FOnTimeToFields;
    property OnInsertItem: TDBItemSQLEvent read FOnInsertItem write FOnInsertItem;
    property OnDeleteItem: TDBItemSQLEvent read FOnDeleteItem write FOnDeleteItem;
    property OnUpdateItem: TDBItemSQLEvent read FOnUpdateItem write FOnUpdateItem;
    property OnCreateKey: TCreateKeyEvent read FOnCreateKey write FOnCreateKey;
  end;

implementation

{ TDBPlannerMonthView }

procedure TDBPlannerMonthView.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;

begin
  inherited;

  if FIsUpdating then
    Exit;

  if not CheckDataSet then
    Exit;

  FIsUpdating := true;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    Items.DBItem.DBKey := CreateKey;
    EndUpdate;
    FOnInsertItem(Self,Items.DBItem);
    BeginUpdate;
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Append;

  if not DataBinding.AutoIncKey then
  begin
    D.FieldByName(DataBinding.KeyField).AsString := CreateKey;
    Items.DBItem.DBKey := D.FieldByName(DataBinding.KeyField).AsString;
  end;

  DTS := Items.DBItem.ItemStartTime;
  DTE := Items.DBItem.ItemEndTime;

  Items.DBItem.ItemRealStartTime := DTS;
  Items.DBItem.ItemRealEndTime := DTE;

  TimeToFields(D.Fields,DTS,DTE);

  if DataBinding.SubjectField <> '' then
    D.FieldByName(DataBinding.SubjectField).AsString := Items.DBItem.CaptionText;

  if DataBinding.NotesField <> '' then
    D.FieldByName(DataBinding.NotesField).AsString := Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, Items.DBItem);

  try
    D.Post;
    if DataBinding.AutoIncKey then
      Items.DBItem.DBKey := D.FieldByName(DataBinding.KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    FIsUpdating := false;    
    EndUpdate;
  end;
end;

function TDBPlannerMonthView.CheckDataSet: Boolean;
begin
  Result := False;

  if DataBinding.KeyField = '' then
    Exit;

  if Assigned(DataSource) then
  begin
    if Assigned(DataSource.DataSet) then
    begin
      if DataSource.DataSet.Active then
      begin
        Result := (((DataBinding.StartTimeField <> '') AND (DataBinding.EndTimeField <> '')) OR
          (Assigned(FOnFieldsToTime) AND Assigned(FOnTimeToFields))) AND (DataBinding.KeyField <> '');
      end;
    end
  end;
end;

procedure TDBPlannerMonthView.ClearDBItems;
begin
  if IsUpdating then
    Exit;
  Items.BeginUpdate;
  Items.ClearDB;
  Items.EndUpdate;
  Items.Selected := nil;
end;

constructor TDBPlannerMonthView.Create(AOwner: TComponent);
begin
  inherited;
  FDataBinding := TPlannerDataBinding.Create(Self);
  FDatalink := TPlannerDataLink.Create(Self);
  FRecurrencyHandler := TRecurrencyHandler.Create;
end;

function TDBPlannerMonthView.CreateItem: TPlannerItem;
begin
  Result := inherited CreateItem;
  Items.DBItem := Result;
  AddDBItem;
end;

function TDBPlannerMonthView.CreateItemAtSelection: TPlannerItem;
begin
  Result := inherited CreateItemAtSelection;
  Items.DBItem := Result;
  AddDBItem;
end;



function TDBPlannerMonthView.CreateKey: string;
var
  GUID: TGUID;
  Key: string;
begin
  Key := '';

  if Assigned(FOnCreateKey) then
    FOnCreateKey(Self,Items.DBItem, Key)
  else
  begin

    CoCreateGUID(GUID);

    Key := GUIDToString(GUID);

  end;

  Result := Key;
end;

procedure TDBPlannerMonthView.DeleteDBItem;
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    EndUpdate;
    FOnDeleteItem(Self,Items.DBItem);
    BeginUpdate;
    EndUpdate;
    Exit;
  end;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(DataBinding.KeyField,Items.DBItem.DBKey,[]);

  D.Delete;
  EndUpdate;
end;

procedure TDBPlannerMonthView.SelectDBItem;
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then
    Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  D := DataSource.DataSet;

  D.Locate(DataBinding.KeyField,Items.DBItem.DBKey,[]);
end;


destructor TDBPlannerMonthView.Destroy;
begin
  FDataBinding.Free;
  FDataLink.Free;
  FRecurrencyHandler.Free;
  inherited;
end;

procedure TDBPlannerMonthView.DoChangeMonth(dt1, dt2: TDateTime);
begin
  inherited;
  Refresh;
end;

procedure TDBPlannerMonthView.DoChangeYear(dt1, dt2: TDateTime);
begin
  inherited;
  Refresh;
end;

procedure TDBPlannerMonthView.FieldsToTime(Fields: TFields; var dtS,
  dtE: TDateTime);
begin
  if Assigned(FOnFieldsToTime) then
  begin
    FOnFieldsToTime(Self,Fields,dtS,dtE);
  end
  else
  begin
    dtS := Fields.FieldByName(DataBinding.StartTimeField).AsDateTime;
    dtE := Fields.FieldByName(DataBinding.EndTimeField).AsDateTime;
  end;
end;

procedure TDBPlannerMonthView.RefreshItem(APlannerItem: TPlannerItem);
var
  EditKey, DBKey: string;
  plIt: TPlannerItem;
begin
  inherited;

  if APlannerItem.NonDBItem then
    Exit;

  Items.DBItem := APlannerItem;

  EditKey := Items.DBItem.DBKey;

  if (Items.DBItem.Recurrent) or (Items.DBItem.Recurrency <> '') then
  begin
    DBKey := Items.DBItem.DBKey;
    ClearDBKey(DBKey);
    ReadDBItems(DBKey);
  end
  else
    ReadDBItem;

  plIt := Items.FindKey(EditKey);
  if Assigned(plIt) then
    Items.Select(plIt);
end;


procedure TDBPlannerMonthView.UpdateItem(APlannerItem: TPlannerItem);
begin
  if not APlannerItem.NonDBItem then
  begin
    Items.DBItem := APlannerItem;
    WriteDBItem;
  end;  
  RefreshItem(APlannerItem);
end;

procedure TDBPlannerMonthView.FreeItem(APlannerItem: TPlannerItem);
begin
  Items.DBItem := APlannerItem;
  FIsUpdating := True;                         
  DeleteDBItem;
  FIsUpdating := False;
  inherited FreeItem(APlannerItem);
end;

procedure TDBPlannerMonthView.ItemChanged(DBKey: string);
var
  plIt: TMonthPlannerItem;
  dtS, dtE: TDateTime;
  sd, ed: TDateTime;
  D: TDataSet;

begin
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;

  plIt := TMonthPlannerItem(Items.FindKey(DBKey));

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}

    Items.DBItem := plIt;

    if plIt.Recurrent then
    begin
      ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      Items.DBItem := nil;
    end
    else
    begin
      ReadDBItem;
      Items.Select(plIt);
    end;  
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then
      Exit;

    D := DataSource.DataSet;

    sd := EncodeDate(Year, Month, 1);
    if Month = 12 then
      ed := EncodeDate(Year + 1, 1, 1) - 1
    else
      ed := EncodeDate(Year, Month + 1, 1) - 1;

    FieldsToTime(D.Fields,dtS,dtE);

    if ((dts >= sd) and (dts <= ed)) or
       ((dte >= sd) and (dte <= ed)) or
       ((dts <= sd) and (dte >= ed)) then
    begin
      plIt := TMonthPlannerItem(Items.Add);
      plIt.Assign(DefaultItem);
      plIt.DBKey := DBKey;
      Items.DBItem := plIt;
      ReadDBItem;
    end;
  end;
  EndUpdate;
end;

procedure TDBPlannerMonthView.ItemEdited(Sender: TObject;
  Item: TPlannerItem);
var
  DBKey: string;
begin
  inherited;

  if Item.NonDBItem then
    Exit;

  Items.DBItem := Item;
  WriteDBItem;
  if Items.DBItem.Recurrent then
  begin
    DBKey := Items.DBItem.DBKey;
    ClearDBKey(DBKey);
    ReadDBItems(DBKey);
  end;
end;

procedure TDBPlannerMonthView.ItemMoved(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime);
var
  DBKey: string;
begin
  inherited;
  
  if not Assigned(Items.Selected) then
    Exit;

  if Items.Selected.NonDBItem then
    Exit;

  BeginUpdate;
  Items.DBItem := APlannerItem;
  WriteDBItem;

  if Items.DBItem.Recurrent then
  begin
    DBKey := Items.DBItem.DBKey;
    ClearDBKey(DBKey);
    ReadDBItems(DBKey);
  end;
  EndUpdate;
end;

procedure TDBPlannerMonthView.ItemSized(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime);
var
  DBKey: string;
begin
  inherited;

  if not Assigned(Items.Selected) then
    Exit;

  if Items.Selected.NonDBItem then
    Exit;

  Items.DBItem := Items.Selected;
  WriteDBItem;
  if Items.DBItem.Recurrent then
  begin
    DBKey := Items.DBItem.DBKey;
    ClearDBKey(DBKey);
    ReadDBItems(DBKey);
  end;
end;

procedure TDBPlannerMonthView.ItemSelected(Item: TPlannerItem);
begin
  Items.DBItem := Item;
  FIsUpdating := true;
  SelectDBItem;
  FIsUpdating := false;
  inherited;
end;

procedure TDBPlannerMonthView.Loaded;
begin
  inherited;
  Items.BeginUpdate;
  ClearDBItems;
  ReadDBItems('');
  Items.EndUpdate;
end;

procedure TDBPlannerMonthView.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FDataSource) then
  begin
    FDataSource := nil;
    FDataLink.DataSource := nil;
  end;
end;

procedure TDBPlannerMonthView.ReadDBItem;
var
  D: TDataSet;
  dts, dte: TDateTime;
  sd, ed: TDateTime;

begin
  // read the DB here
  if CheckDataSet then
  begin
    FIsUpdating := true;

    D := DataSource.DataSet;

    D.DisableControls;

    sd := EncodeDate(Year, Month, 1);
    if Month = 12 then
      ed := EncodeDate(Year + 1, 1, 10) + EncodeTime(23,59,59,99) - 1
    else
      ed := EncodeDate(Year, Month + 1, 1) + EncodeTime(23,59,59,99) - 1;

    Items.BeginUpdate;

    FieldsToTime(D.Fields, dtS, dtE);

    if ((dts >= sd) and (dts <= ed)) or
       ((dte >= sd) and (dte <= ed)) or
       ((dts <= sd) and (dte >= ed)) then
    begin
      with Items.DBItem do
      begin
        ItemStartTime := Int(dts);
        ItemEndTime := Int(dte);
        ItemRealStartTime := dts;
        ItemRealEndTime := dte;
        RealTime := true;

        if DataBinding.SubjectField <> '' then
          CaptionText := D.FieldByName(DataBinding.SubjectField).AsString;

        if DataBinding.NotesField <> '' then
          Text.Text := D.FieldByName(DataBinding.NotesField).AsString;
      end;

      if Assigned(OnFieldsToItem) then
        OnFieldsToItem(Self, D.Fields, Items.DBItem);

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('after read: '+ formatdatetime('dd/mm/yyyy hh:nn',Items.DBItem.ItemRealStartTime)));
      {$ENDIF}
    end;

    Items.EndUpdate;
    Invalidate;

    D.EnableControls;
    FIsUpdating := false;
  end;
end;

procedure TDBPlannerMonthView.ReadDBItems(UpdateKey: string);
var
  B: TBookMark;
  D: TDataSet;
  dts, dte: TDateTime;
  rdts, rdte: TDateTime;
  sd, ed: TDateTime;
  plIt: TMonthPlannerItem;
  da,mo,ye:word;
  i: integer;
begin

  // read the DB here
  if CheckDataSet then
  begin
    FIsUpdating := true;

    D := DataSource.DataSet;

    B := D.GetBookMark;
    try
      D.DisableControls;

      i := 0;

      D.First;

      GetStartDate(da,mo,ye);

      sd := EncodeDate(ye,mo,da);

      GetEndDate(da,mo,ye);

      ed := EncodeDate(ye,mo,da) + EncodeTime(23,59,59,99);

      //sd := EncodeDate(Year, Month, 1);
      //if Month = 12 then
      //  ed := EncodeDate(Year + 1, 1, 1) + EncodeTime(23,59,59,99) - 1
      //else
      //  ed := EncodeDate(Year, Month + 1, 1) + EncodeTime(23,59,59,99) - 1;

      Items.BeginUpdate;

      while not D.Eof do
      begin
        if (UpdateKey = '') or (UpdateKey = D.FieldByName(DataBinding.KeyField).AsString) then
        begin
          FieldsToTime(D.Fields, dtS, dtE);
          rdtS := dtS;
          rdtE := dtE;

          if DataBinding.RecurrencyField <> '' then
            FRecurrencyHandler.Recurrency := D.FieldByName(DataBinding.RecurrencyField).AsString;

          FRecurrencyHandler.StartTime := dts;
          FRecurrencyHandler.EndTime := dte;
          FRecurrencyHandler.TimeSpan := ed;
          FRecurrencyHandler.Generate;

          while FRecurrencyHandler.NextDate(dts,dte) do
          begin
             if ((dts >= sd) and (dts <= ed)) or
               ((dte >= sd) and (dte <= ed)) or
               ((dts <= sd) and (dte >= ed)) then
            begin
              plIt := TMonthPlannerItem(CreateItem);

              with plIt do
              begin;
                ItemStartTime := Int(dts);
                ItemEndTime := Int(dte);

                Recurrent := FRecurrencyHandler.IsRecurrent;
                Recurrency := FRecurrencyHandler.Recurrency;

                RecurrentOrigStart := dts;
                RecurrentOrigEnd := dte;

                RecurrentStart := rdts;
                RecurrentEnd := rdte;

                ItemRealStartTime := dts;
                ItemRealEndTime := dte;
                RealTime := true;

                Tag := i;
                inc(i);

                DBKey := D.FieldByName(DataBinding.KeyField).AsString;

                if DataBinding.SubjectField <> '' then
                  CaptionText := D.FieldByName(DataBinding.SubjectField).AsString;

                if DataBinding.NotesField <> '' then
                  Text.Text := D.FieldByName(DataBinding.NotesField).AsString;
              end;

              if Assigned(OnFieldsToItem) then
                OnFieldsToItem(Self, D.Fields, plIt);
            end;

          end;
        end;
        D.Next;
      end;

      Items.EndUpdate;

    finally
      D.GotoBookMark(B);
      D.FreeBookMark(B);
      D.EnableControls;
      FIsUpdating := false;
    end;
  end;
end;

procedure TDBPlannerMonthView.Refresh;
begin
  Items.Clear;
  ReadDBItems('');
end;

procedure TDBPlannerMonthView.SetDataBinding(
  const Value: TPlannerDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TDBPlannerMonthView.SetDataSource(const Value: TDataSource);
begin
  FDataSource := Value;
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
  begin
    Items.Clear;
    ReadDBItems('');
  end;
end;

procedure TDBPlannerMonthView.TimeToFields(Fields: TFields; dtS,
  dtE: TDateTime);
begin
  if Assigned(FOnTimeToFields) then 
  begin
    FOnTimeToFields(Self,Fields,dtS,dtE);
  end
  else
  begin
    Fields.FieldByName(DataBinding.StartTimeField).AsDateTime := dtS;
    Fields.FieldByName(DataBinding.EndTimeField).AsDateTime := dtE;
  end;
end;

procedure TDBPlannerMonthView.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTE, DTS: TDateTime;
  deltastart, deltaend: TDateTime;  

begin
  if IsUpdating then
    Exit;

  if not CheckDataSet then
    Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,Items.DBItem);
    EndUpdate;
    if not DataBinding.UpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  FIsUpdating := true;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Locate(DataBinding.KeyField,Items.DBItem.DBKey,[]);

  D.Edit;

  if Items.DBItem.RealTime then
  begin
    DTS := Items.DBItem.ItemRealStartTime;
    DTE := Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := Int(Items.DBItem.ItemStartTime) + Frac(Items.DBItem.ItemRealStartTime);
    DTE := Int(Items.DBItem.ItemEndTime) + Frac(Items.DBItem.ItemRealEndTime);
  end;

  if Items.DBItem.Recurrent then
  begin
    deltastart := DTS - Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - Items.DBItem.RecurrentOrigEnd;

    DTS := Items.DBItem.RecurrentStart + deltastart;
    DTE := Items.DBItem.RecurrentEnd + deltaend;
  end;


  {$IFDEF TMSDEBUG}
  Outputdebugstring(pchar('writedb:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte)));
  {$ENDIF}

  Items.DBItem.RealTime := True;

  //  if DTE < DTS then
  //    DTE := DTE + 1;

  TimeToFields(D.Fields,DTS,DTE);

  if (DataBinding.SubjectField <> '') and
     (D.FieldByName(DataBinding.SubjectField).CanModify) then
    D.FieldByName(DataBinding.SubjectField).AsString := Items.DBItem.CaptionText;

  if (DataBinding.NotesField <> '') and
     (D.FieldByName(DataBinding.NotesField).CanModify) then
    D.FieldByName(DataBinding.NotesField).AsString := Items.DBItem.ItemText;

  if (DataBinding.RecurrencyField <> '') and
     (D.FieldByName(DataBinding.RecurrencyField).CanModify) then
    D.FieldByName(DataBinding.RecurrencyField).AsString := Items.DBItem.Recurrency; 

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    FIsUpdating := False;    
    EndUpdate;
  end;
end;

procedure TDBPlannerMonthView.ClearDBKey(Key: string);
var
  i: Integer;
begin
  i := 0;

  while (i < Items.Count) do
  begin
    if Items[i].DBKey = Key then
      Items[i].Free
    else
      inc(i);
  end;
end;

function TDBPlannerMonthView.CreateNonDBItem: TPlannerItem;
begin
  Result := inherited CreateItem;
  Result.NonDBItem := True;
end;

procedure TDBPlannerMonthView.SetDateProc(const Value: TDatetime);
begin
  inherited;
  Refresh;
end;

{ TPlannerDataBinding }

procedure TPlannerDataBinding.Assign(Source: TPersistent);
begin
  if (Source is TPlannerDataBinding) then
  begin
    FAutoIncKey := (Source as TPlannerDataBinding).AutoIncKey;
    FUpdateByQuery := (Source as TPlannerDataBinding).UpdateByQuery;
    FStartTimeField := (Source as TPlannerDataBinding).StartTimeField;
    FEndTimeField := (Source as TPlannerDataBinding).EndTimeField;
    FKeyField := (Source as TPlannerDataBinding).KeyField;
    FSubjectField := (Source as TPlannerDataBinding).SubjectField;
    FNotesField := (Source as TPlannerDataBinding).NotesField;
  end;
end;

constructor TPlannerDataBinding.Create(AOwner: TDBPlannerMonthView);
begin
  inherited Create;
  FPlannerMonthView := AOwner;
end;

procedure TPlannerDataBinding.SetEndTimeField(const Value: string);
begin
  FEndTimeField := Value;
end;

procedure TPlannerDataBinding.SetKeyField(const Value: string);
begin
  FKeyField := Value;
end;

procedure TPlannerDataBinding.SetMaxTimeField(const Value: string);
begin
  FMaxTimeField := Value;
end;

procedure TPlannerDataBinding.SetMinTimeField(const Value: string);
begin
  FMinTimeField := Value;
end;

procedure TPlannerDataBinding.SetNotesField(const Value: string);
begin
  FNotesField := Value;
end;

procedure TPlannerDataBinding.SetRecurrencyField(const Value: string);
begin
  FRecurrencyField := Value;
end;

procedure TPlannerDataBinding.SetStartTimeField(const Value: string);
begin
  FStartTimeField := Value;
end;

procedure TPlannerDataBinding.SetSubjectField(const Value: string);
begin
  FSubjectField := Value;
end;

{ TPlannerDataLink }

procedure TPlannerDataLink.ActiveChanged;
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('ActiveChanged');
  {$ENDIF}
  FDBPlannerMonthView.ClearDBItems;
  FDBPlannerMonthView.ReadDBItems('');
  FDBPlannerMonthView.Refresh;
end;


constructor TPlannerDataLink.Create(ADBPlannerMonthView: TDBPlannerMonthView);
begin
  inherited Create;
  FDBPlannerMonthView := ADBPlannerMonthView;
end;

procedure TPlannerDataLink.DataSetChanged;
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('DatasetChanged');
  {$ENDIF}
  if FDBPlannerMonthView.IsUpdating then
    Exit;

  if Assigned(DataSet) then
    if DataSet.Active then
    begin
      if DataSet.State = dsBrowse then
        FDBPlannerMonthView.Refresh;
    end;
end;

procedure TPlannerDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('DatasetScrolled');
  {$ENDIF}
end;

destructor TPlannerDataLink.Destroy;
begin
  inherited;
end;

procedure TPlannerDataLink.RecordChanged(Field: TField);
begin
  inherited;

  if FDBPlannerMonthView.IsUpdating then
    Exit;

  {what is the connected planner ??}

  if Assigned(DataSet) then
    if DataSet.Active then
    begin
      {$IFDEF TMSCODESITE}
      if Assigned(Field) then
       SendMsg('RecordChanged : '+Field.FieldName +' of '+DataSet.FieldByName(FDBItemSource.KeyField).AsString)
      else
       SendMsg('RecordChanged : '+DataSet.FieldByName(FDBItemSource.KeyField).AsString);
      {$ENDIF}
      if DataSet.State = dsBrowse then
        FDBPlannerMonthView.ItemChanged(DataSet.FieldByName(FDBPlannerMonthView.DataBinding.KeyField).AsString);
    end;
end;


end.
