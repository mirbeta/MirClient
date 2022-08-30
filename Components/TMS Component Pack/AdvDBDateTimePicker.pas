{***********************************************************************}
{ TAdvDBDateTimePicker component                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2007 - 2012                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvDBDateTimePicker;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls, AdvDateTimePicker;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDBDateTimePicker = class(TAdvDateTimePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FOldState: TDataSetState;
    FIsEditing: Boolean;
    FInternalCall: boolean;
    FNewDate: TDateTime;
    FNullDate: Boolean;
    FInternalChange: Boolean;
    FBeingChanged: Boolean;
    FOldSet: Boolean;
    FOldDateTime: TDateTime;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure HandleKey(Key: word);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function EditCanModify: Boolean; virtual;
    procedure DoExit; override;
    procedure TimePickerChanged; override;
    procedure TimePickerKeyPress(Sender: TObject; var Key: Char); override;
    procedure SetDateTimeEx(const Value: TDateTime); override;
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
  end;

implementation

uses
  CommCtrl, ComCtrls;

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


//------------------------------------------------------------------------------

{ TAdvDBDateTimePicker }

procedure TAdvDBDateTimePicker.Change;
var
  d: TDateTime;
begin
  FBeingChanged := True;
  try
    if self.Text = '' then
    begin
      FNullDate := true;
      if not FInternalCall then
        if EditCanModify then
        begin
          FNewDate := -1;
          FDataLink.Modified;
        end;
    end
    else
    begin
      // Workaround to fix TDateTimePicker's issue ie: trigers OnChange event twice
      if not FOldSet or (DateTime <> FOldDateTime) then
      begin
        inherited;
        FOldSet := True;
        FOldDateTime := DateTime;
      end;
      //
      
      FNullDate := not Checked;
      d := self.DateTime;
      if not FInternalCall then
      begin
        if EditCanModify then
        begin
          FNewDate := d;
          FDataLink.Modified;
        end;
        //else
          //DataChange(FDataLink.DataSet);

        if ShowCheckbox and Assigned(FTimePicker) then
          FTimePicker.Enabled := Checked;
      end;
    end;
  finally
    FBeingChanged := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

//------------------------------------------------------------------------------

constructor TAdvDBDateTimePicker.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;

  FInternalCall:= false;
  FNewDate:= Self.DateTime;
  FNullDate:= false;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.DataChange(Sender: TObject);
begin

  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing or FBeingChanged then
    Exit;

  FInternalChange := True;
  if Assigned(FDataLink.Field) then
  begin
    if not (FDataLink.DataSet.State = dsInsert) then
    begin
      if (FOldState <> dsInsert)  then
      begin
        FInternalCall:= true;
        if FDataLink.Field.AsString = '' then
        begin
          self.DateTime := 0;
          self.Text := '';
          if ShowCheckbox then
            Checked := False;
        end
        else
        begin
          self.DateTime := FDataLink.Field.AsDateTime;
        end;
        FInternalCall:= false;
      end
      else
      begin
        self.DateTime := FDataLink.Field.AsDateTime;
        FNewDate := self.DateTime;
      end;

      if ShowCheckbox and Assigned(FTimePicker) then
        FTimePicker.Enabled := Checked;
    end;

    if (FDataLink.DataSet.State = dsInsert) then
    begin
      self.DateTime := FDataLink.Field.AsDateTime;
      FNewDate := self.DateTime;
    end;
  end;

  FOldState := FDataLink.DataSet.State;
  FInternalChange := False;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Editing then
  begin
    if FNullDate then
      FDataLink.Field.AsString:= ''
    else
      FDataLink.Field.AsDateTime := FNewDate;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvDBDateTimePicker.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.DoExit;
begin
  inherited;
  if FDataLink.Editing then
  begin
    FDataLink.UpdateRecord;//DataUpdate(Self);
  end;
  
end;

//------------------------------------------------------------------------------

function TAdvDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TAdvDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.Loaded;
begin
  inherited Loaded;
  if (Kind = dkDateTime) and not Assigned(FTimePicker) then
    CreateTimePicker;
  DateTime := DateTime;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.ActiveChange(Sender: TObject);
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

//------------------------------------------------------------------------------

function TAdvDBDateTimePicker.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TAdvDBDateTimePicker.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

function TAdvDBDateTimePicker.EditCanModify: Boolean;
begin
  FDataLink.OnDataChange := nil;
  try
    Result := FDataLink.Edit;
  finally
    FDataLink.OnDataChange := DataChange;
end;

end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.TimePickerChanged;
begin
  Change;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  HandleKey(Message.CharCode);
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.TimePickerKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  HandleKey(Integer(key));
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.HandleKey(Key: word);
begin
  if (key = VK_ESCAPE) then
  begin
    if FDataLink.Editing and Assigned(FDataLink.DataSet) and (FDataLink.DataSet.Active) then
    begin
      if Assigned(FDataLink.Field) then
        FNewDate := FDataLink.Field.AsDateTime;
      FDataLink.Reset;
      //FDataLink.DataSet.Cancel;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.SetDateTimeEx(const Value: TDateTime);
var
  Oldvalue: TDateTime;
begin
  Oldvalue := DateTime;
  inherited;
  if not (csLoading in ComponentState) and not FInternalChange then
    Change                               
  else if not (csLoading in ComponentState) and (DateTime <> OldValue) and Assigned(OnChange) then
    OnChange(Self);        
end;

//------------------------------------------------------------------------------

procedure TAdvDBDateTimePicker.CNNotify(var Message: TWMNotify);
begin
  inherited;
  with Message, NMHdr^ do
  begin
    if (code = DTN_CLOSEUP) then
    begin
      if not (FDataLink.DataSet.State in [dsEdit, dsInsert]) then
        DataChange(FDataLink.DataSet);
    end;
  end;
end;
//------------------------------------------------------------------------------


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}



end.
