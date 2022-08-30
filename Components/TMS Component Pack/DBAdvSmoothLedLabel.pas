{***************************************************************************}
{ TDBAdvSmoothLedLabel component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
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

unit DBAdvSmoothLedLabel;

interface

uses
  Classes, Messages, Controls, DB, DBCtrls, VDBConsts, AdvSmoothLedLabel;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSmoothLedLabel = class(TAdvSmoothLedLabel)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: WideString;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldValue: Double;
    function GetFieldTimeValue: TDateTime;
    procedure SetDataField(const Value: WideString);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: WideString read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Transparent;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Windows;

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


constructor TDBAdvSmoothLedLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  AutoSize := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TDBAdvSmoothLedLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBAdvSmoothLedLabel.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBAdvSmoothLedLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

function TDBAdvSmoothLedLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvSmoothLedLabel.SetDataSource(Value: TDataSource);
begin
  if not(FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TDBAdvSmoothLedLabel.GetDataField: WideString;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBAdvSmoothLedLabel.SetDataField(const Value: WideString);
begin
  FDataLink.FieldName := Value;
end;

function TDBAdvSmoothLedLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBAdvSmoothLedLabel.GetFieldTimeValue: TDateTime;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.AsDateTime
  else if csDesigning in ComponentState then
    Result := 0
  else
    Result := 0;
end;

function TDBAdvSmoothLedLabel.GetFieldValue: Double;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.AsFloat
  else if csDesigning in ComponentState then
    Result := 0
  else
    Result := 0;
end;

procedure TDBAdvSmoothLedLabel.DataChange(Sender: TObject);
begin
  case Caption.ValueType of
    vtNormal: Caption.Value := GetFieldValue;
    vtDateTime: Caption.TimeValue := GetFieldTimeValue;
  end;
end;

procedure TDBAdvSmoothLedLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

function TDBAdvSmoothLedLabel.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil)
    and FDataLink.ExecuteAction(Action);
end;

function TDBAdvSmoothLedLabel.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil)
    and FDataLink.UpdateAction(Action);
end;

end.
