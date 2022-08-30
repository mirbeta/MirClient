{***********************************************************************}
{ TPlannerDBMaskDatePicker component                                    }{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit PlannerDBMaskDatePicker;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls, PlannerMaskDatePicker;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerDBMaskDatePicker = class(TPlannerMaskDatePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FOldState: TDataSetState;
    FIsEditing: Boolean;
    FInternalCall: boolean;
    FNewDate: TDateTime;
    FNullDate: Boolean;
    FDonNotCallChange: boolean;
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
   // procedure Loaded; override;
    function CanModify: Boolean; virtual;

    procedure DaySelect; override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure DoExit; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation
uses
  Mask;

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



{ TPlannerDBMaskDatePicker }

procedure TPlannerDBMaskDatePicker.Change;
var
  d: TDateTime;
  EdM: string;
  EdT: string;
  SS,SL: integer;
  nd: TDateTime;
begin
  if FDonNotCallChange then
    Exit;

  EdT := EditText;
  EdM := EditMask;
  FDonNotCallChange := true;
  nd := self.Date;

  SS := SelStart;
  SL := SelLength;

  { get text without the edit mask }
  EditMask := '';

  if self.Text = '' then
  begin
    FNullDate := true;
    if not FInternalCall then
      if CanModify then
      begin
        FNewDate := -1;
        FDataLink.Modified;
      end;
  end
  else
  begin
    inherited;

    FNullDate := false;
    d := nd;
    if not FInternalCall then
      if CanModify then
      begin
        FNewDate := d;
        FDataLink.Modified;
      end;
  end;

  EditMask := EdM;
  EditText := EdT;
  SelStart := SS;
  SelLength := SL;
  FDonNotCallChange := false;
end;

procedure TPlannerDBMaskDatePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

procedure TPlannerDBMaskDatePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

constructor TPlannerDBMaskDatePicker.Create(aOwner: TComponent);
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
  FDonNotCallChange:= false;
end;

procedure TPlannerDBMaskDatePicker.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing then
    Exit;

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
    end;

    if (FDataLink.DataSet.State = dsInsert) {and (FOldState <> dsInsert)} then
    begin
      self.Date := FDataLink.Field.AsDateTime;
    end;
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TPlannerDBMaskDatePicker.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) {and not (FDataLink.DataSet.State = dsInsert)} then
  begin
    if FNullDate then
      FDataLink.Field.AsString:= ''
    else
      FDataLink.Field.AsDateTime := FNewDate;//self.Date;
  end;
end;

destructor TPlannerDBMaskDatePicker.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;


function TPlannerDBMaskDatePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TPlannerDBMaskDatePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TPlannerDBMaskDatePicker.GetReadOnly: Boolean;
begin
  //Result := FDataLink.ReadOnly;
  Result:= FDataLink.ReadOnly;
end;

procedure TPlannerDBMaskDatePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TPlannerDBMaskDatePicker.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TPlannerDBMaskDatePicker.SetReadOnly(Value: Boolean);
begin
  //FDataLink.ReadOnly := Value;
  inherited Readonly:= Value;
end;
{
procedure TPlannerDBMaskDatePicker.Loaded;
begin
  inherited Loaded;
end;
}
procedure TPlannerDBMaskDatePicker.ActiveChange(Sender: TObject);
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


function TPlannerDBMaskDatePicker.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TPlannerDBMaskDatePicker.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


function TPlannerDBMaskDatePicker.CanModify: Boolean;
begin
  FDataLink.OnDataChange := nil;
  Result := FDataLink.Edit;
  FDataLink.OnDataChange := DataChange;
end;

procedure TPlannerDBMaskDatePicker.DoExit;
var  EdM, OldText: string;

begin  if Assigned(OnExit) then    OnExit(Self);  EdM := EditMask;

  FDonNotCallChange:= true;
  OldText := Text;
  EditMask := '';
  if Text <> '' then
  begin
    //inherited;
    try
      FNewDate := StrToDate(OldText);
  //    Calendar.Date := dt;
    except
    end;
  end;
  EditMask := EdM;
  //Text := OldText;
  FDonNotCallChange:= false;
end;

procedure TPlannerDBMaskDatePicker.DaySelect;
var  EdM: string;
  nd: TDateTime;
begin
  nd := self.Date;
  EdM := EditMask;
  EditMask := '';
  if self.Text = '' then
  begin
    FNullDate := true;
    if not FInternalCall then
      if CanModify then
      begin
        FNewDate:= -1;
        FDataLink.Modified;
      end;
  end
  else
  begin
    inherited;
    FNullDate := false;
    if not FInternalCall then
      if CanModify then
      begin
        FNewDate := nd;
        FDataLink.Modified;
      end;
  end;
  EditMask := EdM;
end;

end.
