{***************************************************************************}
{ TDBAdvGraphicCheckLabel component                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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
unit DBAdvGraphicCheckLabel;

interface

uses
  Classes, Windows, Messages, DB, DBCtrls, DBConsts, Controls, AdvGraphicCheckLabel;

{$I TMSDEFS.INC}

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvGraphicCheckLabel = class(TAdvGraphicCheckLabel)
  private
    FDataLink: TFieldDataLink;
    FValueUncheck: string;
    FValueCheck: string;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetFieldState: boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    { Protected declarations }
    procedure Toggle; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ValueChecked: string read FValueCheck write SetValueCheck;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck;
  end;

implementation

uses
  SysUtils;

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

{ TDBAdvGraphicCheckLabel }

procedure TDBAdvGraphicCheckLabel.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

constructor TDBAdvGraphicCheckLabel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  Checked := false;
  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.DataChange(Sender: TObject);
begin
  Checked := GetFieldState;
end;

//------------------------------------------------------------------------------

destructor TDBAdvGraphicCheckLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TDBAdvGraphicCheckLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TDBAdvGraphicCheckLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TDBAdvGraphicCheckLabel.GetFieldState: boolean;
var
  Text: string;
begin
  if Assigned(FDatalink.Field) then
  begin
    if FDataLink.Field.DataType = ftBoolean then
      Result := FDataLink.Field.AsBoolean
    else
    begin
      Text := FDataLink.Field.Text;
      Result := ValueMatch(FValueCheck, Text);
    end
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

function TDBAdvGraphicCheckLabel.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.Toggle;
begin
  if FDataLink.Edit then
  begin
    inherited Toggle;
    FDataLink.Modified;
    FDataLink.UpdateRecord;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGraphicCheckLabel.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := Checked
  else
  begin
    if Checked then
      S := FValueCheck
    else
      S := FValueUncheck;

    Pos := 1;
    {$IFDEF DELPHI9_LVL}
    FDataLink.Field.Text := ExtractFieldName(WideString(S), Pos);
    {$ELSE}
    FDataLink.Field.Text := ExtractFieldName(S, Pos);
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGraphicCheckLabel.ValueMatch(const ValueList,
  Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    {$IFDEF DELPHI9_LVL}
    if AnsiCompareText(ExtractFieldName(WideString(ValueList), Pos), Value) = 0 then
    {$ELSE}
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    {$ENDIF}
    begin
      Result := True;
      Break;
    end;
end;


end.
