{***************************************************************************}
{ TDBAdvSmoothSlider component                                              }
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

unit DBAdvSmoothSlider;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Controls, SysUtils, AdvSmoothSlider, Messages, DB,
  DbCtrls, DBConsts;

type
  TDBAdvSmoothSlider = class(TAdvSmoothSlider)
  private
    BlockUpdate: Boolean;
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldState: TAdvSmoothSliderState;
    function IsValueChecked: Boolean;
    function IsValueUnchecked: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure UpdateData(Sender: TObject);
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure DoStateChanged(State: TAdvSmoothSliderState; Value: Double); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property State;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ValueChecked: string read FValueCheck write SetValueCheck stored IsValueChecked nodefault;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck stored IsValueUnchecked nodefault;
  end;

implementation

{ TDBAdvSmoothSlider }

constructor TDBAdvSmoothSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  State := ssOff;
  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDBAdvSmoothSlider.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBAdvSmoothSlider.DoStateChanged(State: TAdvSmoothSliderState;
  Value: Double);
begin
  BlockUpdate := True;
  if FDataLink.Edit then
  begin
    inherited;
    FDataLink.Modified;
  end;
  BlockUpdate := False;
end;

procedure TDBAdvSmoothSlider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBAdvSmoothSlider.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TDBAdvSmoothSlider.GetFieldState: TAdvSmoothSliderState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := ssOff
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := ssOn
      else
        Result := ssOff
    else
    begin
      Result := ssOff;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := ssOn else
        if ValueMatch(FValueUncheck, Text) then Result := ssOff;
    end
  else
    Result := ssOn;
end;

procedure TDBAdvSmoothSlider.DataChange(Sender: TObject);
begin
  if not BlockUpdate then
    State := GetFieldState;
end;

procedure TDBAdvSmoothSlider.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := State = ssOn
  else
  begin
    if State = ssOn then S := FValueCheck else S := FValueUncheck;
    Pos := 1;
    FDataLink.Field.Text := ExtractFieldName(S, Pos);
  end;
end;

function TDBAdvSmoothSlider.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TDBAdvSmoothSlider.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvSmoothSlider.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBAdvSmoothSlider.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBAdvSmoothSlider.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDBAdvSmoothSlider.IsValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, STextTrue);
end;

function TDBAdvSmoothSlider.IsValueUnchecked: Boolean;
begin
  Result := not SameText(FValueUncheck, STextFalse);
end;

function TDBAdvSmoothSlider.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBAdvSmoothSlider.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TDBAdvSmoothSlider.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TDBAdvSmoothSlider.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

procedure TDBAdvSmoothSlider.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TDBAdvSmoothSlider.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

function TDBAdvSmoothSlider.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvSmoothSlider.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


end.
