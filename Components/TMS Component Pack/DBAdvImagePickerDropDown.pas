{***************************************************************************}
{ TDBAdvImagePickerDropDown component                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit DBAdvImagePickerDropDown;

interface

uses
  AdvImagePickerDropDown, Windows, Classes, DB, DBCtrls, Messages, Controls;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvImagePickerDropDown = class(TAdvImagePickerDropDown)
  private
    FDataLink: TFieldDataLink;
    FOldState: TDataSetState;
    FInternalCall: Boolean;
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function EditCanModify: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSelect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
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


{ TDBAdvDropDown }

procedure TDBAdvImagePickerDropDown.ActiveChange(Sender: TObject);
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

procedure TDBAdvImagePickerDropDown.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

constructor TDBAdvImagePickerDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  OnSelect := DoSelect;

  FInternalCall:= false;
end;

procedure TDBAdvImagePickerDropDown.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if Assigned(FDataLink.Field) then
  begin
    if not (FDataLink.DataSet.State = dsInsert) then
    begin
      if (FOldState <> dsInsert)  then
      begin
        FInternalCall:= true;
        if FDataLink.Field.AsString = '' then
        begin
          ItemIndex := -1;
        end
        else
        begin
          ItemIndex := FDataLink.Field.AsInteger;
        end;
        FInternalCall:= false;
      end
      else
      begin
        ItemIndex := FDataLink.Field.AsInteger;
      end;
    end;

    if (FDataLink.DataSet.State = dsInsert) then
    begin
      ItemIndex := FDataLink.Field.AsInteger;
    end;
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TDBAdvImagePickerDropDown.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Editing then
  begin
    FDataLink.Field.AsInteger := ItemIndex;
  end;
end;

destructor TDBAdvImagePickerDropDown.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited;
end;

procedure TDBAdvImagePickerDropDown.DoSelect(Sender: TObject);
begin
  inherited;
  if not FInternalCall then
  begin
    if Assigned(FDataLink) then
    begin
      if EditCanModify then
      begin
        FDataLink.Modified;
      end;
    end;
  end;
end;

function TDBAdvImagePickerDropDown.EditCanModify: Boolean;
begin
  Result := true;
  if Assigned(FDataLink) then
  begin
    FDataLink.OnDataChange := nil;
    try
      Result := FDataLink.Edit;
    finally
      FDataLink.OnDataChange := DataChange;
    end;
  end;
end;

function TDBAdvImagePickerDropDown.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvImagePickerDropDown.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvImagePickerDropDown.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvImagePickerDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDBAdvImagePickerDropDown.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvImagePickerDropDown.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBAdvImagePickerDropDown.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.
