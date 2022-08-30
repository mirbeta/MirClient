{***********************************************************************}
{ TPlannerDBDatePicker component                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2014                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit PlannerDBDatePicker;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls, PlannerDatePicker;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerDBDatePicker = class(TPlannerDatePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FOldState: TDataSetState;
    FIsEditing: Boolean;
    FInternalCall: boolean;
    FNewDate: TDateTime;
    FNullDate: Boolean;
    FOrigValue: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function EditCanModify: Boolean; virtual;
    procedure DaySelect; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;



{ TPlannerDBDatePicker }

procedure TPlannerDBDatePicker.Change;
var
  d: TDateTime;
begin
  if self.Text = '' then
  begin
    FNullDate := true;
    if not FInternalCall then
      if EditCanModify then
      begin
        FNewDate := -1;
        FIsEditing := true;
        FDataLink.Field.AsString := '';
        FIsEditing := false;
      end;
  end
  else
  begin
    inherited;

    FNullDate := false;
    d := self.Date;
    if not FInternalCall then
      if EditCanModify then
      begin
        FNewDate := d;
        FIsEditing := true;
        FDataLink.Field.AsDateTime := FNewDate;
        FIsEditing := false;
      end;
  end;
end;

procedure TPlannerDBDatePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

procedure TPlannerDBDatePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

constructor TPlannerDBDatePicker.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;

  FInternalCall:= false;
  FNewDate:= Self.Date;
  FNullDate:= false;
end;

procedure TPlannerDBDatePicker.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing then
    Exit;

  if Assigned(FDataLink) and Assigned(FDataLink.DataSet) and Assigned(FDataLink.Field)
     and (FDataLink.DataSet.State = dsBrowse) then
  begin
    FOrigValue := FDataLink.Field.AsString;
  end;

  if Assigned(FDataLink.Field) then
  begin
    if not (FDataLink.DataSet.State = dsInsert) and
     (FOldState <> dsInsert)  then
    begin
      FInternalCall:= true;
      if FDataLink.Field.AsString = '' then
      begin
        self.Date:= -1;
        self.Text:= '';
      end
      else
      begin
        self.Date := FDataLink.Field.AsDateTime;
      end;
      FInternalCall:= false;
      //Modified := False;
    end;

    if (FDataLink.DataSet.State = dsInsert) {and (FOldState <> dsInsert)} then
    begin
      self.Date := FDataLink.Field.AsDateTime;
    end;
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TPlannerDBDatePicker.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) {and not (FDataLink.DataSet.State = dsInsert)} then
  begin
    if FNullDate then
    begin
      FDataLink.Field.AsString:= '';
    end
    else
    begin
      FDataLink.Field.AsDateTime := FNewDate;//self.Date;
    end;
  end;
end;

destructor TPlannerDBDatePicker.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;


procedure TPlannerDBDatePicker.DoEnter;
begin
  inherited;
  FOrigValue := Text;
end;

procedure TPlannerDBDatePicker.DoExit;
begin
  inherited;
  if (Text <> FOrigValue) then
  begin
    if FDataLink.Edit then
    begin
    	DaySelect();
      DataUpdate(Self);
    end;
  end;
end;

function TPlannerDBDatePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TPlannerDBDatePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TPlannerDBDatePicker.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly or (inherited ReadOnly);
end;

procedure TPlannerDBDatePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TPlannerDBDatePicker.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TPlannerDBDatePicker.SetReadOnly(Value: Boolean);
begin
  //FDataLink.ReadOnly := Value;
  inherited Readonly:= Value;
end;

procedure TPlannerDBDatePicker.Loaded;
begin
  inherited Loaded;
end;


procedure TPlannerDBDatePicker.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
        Text := '';
    end
    else
    begin
      Text := '';
    end;
  end;
end;

function TPlannerDBDatePicker.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TPlannerDBDatePicker.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

function TPlannerDBDatePicker.EditCanModify: Boolean;
begin
  FDataLink.OnDataChange := nil;
  Result := FDataLink.Edit;
  FDataLink.OnDataChange := DataChange;
end;

procedure TPlannerDBDatePicker.DaySelect;
var
  d: TDateTime;
begin
  inherited;
  if self.Text = '' then
  begin
    FNullDate:= true;
    if not FInternalCall then
      if EditCanModify then
      begin
        FNewDate:= -1;
        FDataLink.Modified;
      end;
  end
  else
  begin
    FNullDate:= false;
    d:= self.Date;
    if not FInternalCall then
      if EditCanModify then
      begin
        FNewDate:= d;
        FDataLink.Modified;
      end;
  end;

end;

end.
