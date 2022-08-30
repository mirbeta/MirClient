{***************************************************************************}
{ TDBAdvMemo component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}
{$I TMSDEFS.INC}
unit DBAdvMemo;

interface

uses
  Classes, AdvMemo, DB, DBCtrls, Graphics, Windows, Messages;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvMemo = class(TAdvMemo)
  private
    FDataLink: TFieldDataLink;
    FDBUpdate: Boolean;
    procedure ActiveChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DataUpdate(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    function EditCanModify: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUpdate; virtual;
  published
    {TControl}
    property PopupMenu;
    {TCustomControl}
    property AcceptFiles;
    property ActiveLineSettings;
    property Align;
    property Anchors;
    property AutoCompletion;
    property AutoCompletionListImages;    
    property AutoHintParameterDelay;    
    property AutoHintParameters;
    property AutoHintParameterPosition;
    property AutoExpand;    
    property AutoIndent;
    property AutoThemeAdapt;    
    property BkColor default clWhite;
    property BlockShow;
    property BlockColor;
    property BlockLineColor;
    property BorderStyle;
    property BreakpointColor;
    property BreakpointTextColor;
    property CaseSensitive;
    property Ctl3D;
    property Cursor;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DelErase;
    property Enabled;
    property EnhancedHomeKey;
    property Font;
    property Gutter;
    property HiddenCaret;
    property MarkerList;    
    property MemoChecker;
    property ShowHint;
    property PrintOptions;
    property Visible;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RightMargin;
    property RightMarginColor;
    property ScrollBars;
    property ScrollMode;
    property SelColor;
    property SelBkColor;
    property SyntaxStyles;
    property TabOrder;
    property TabSize;
    property TabStop;
    property TrimTrailingSpaces;
    property UrlAware;
    property UrlStyle;
    property UseStyler;
    property UndoLimit;
    property WantTab;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    property OnGutterClick;
    property OnGutterDraw;
    property OnChange;
    property OnSelectionChange;
    property OnStatusChange;
    property OnUndoChange;
    property OnURLClick;
    property OnStartAutoCompletion;
    property OnAutoCompletion;
    property OnCancelAutoCompletion;
    
  end;

implementation

{ TDBAdvMemo }
                       
procedure TDBAdvMemo.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
      begin
        Lines.BeginUpdate;
        Clear;
      	Lines.EndUpdate;
      end;	 
    end
    else
    begin
      Lines.BeginUpdate;
      Clear;
      Lines.EndUpdate;
    end;
  end;

end;

constructor TDBAdvMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  FDBUpdate := False;
  DelErase := True;
end;

procedure TDBAdvMemo.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if Assigned(FDataLink.Field) and not FDBUpdate then
  begin
//    SendMessage(Handle,WM_SETREDRAW,Integer(False),0);
    Lines.BeginUpdate;
    Lines.Text := FDataLink.Field.AsString;
    DoWrap;
    Lines.EndUpdate;
    LinesChanged(nil);
    CurX := 0;
    CurY := 0;
    ClearSelection;
//    SendMessage(Handle,WM_SETREDRAW,Integer(True),0);
    Refresh;
    Modified := false;
  end;
end;

procedure TDBAdvMemo.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    FDataLink.Field.AsString := Lines.Text;
end;

destructor TDBAdvMemo.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TDBAdvMemo.EditCanModify: Boolean;
begin
  Result := True;

  if not Assigned(FDataLink.DataSet) then
    Exit;

  FDBUpdate := True;

  try
    if not (FDataLink.DataSet.State in [dsEdit,dsInsert]) then
      Result := FDataLink.Edit
    else
      Result := True;

    if Result then
    begin
      DataUpdate(Self);
      FDataLink.Modified;
    end;

  finally
    FDBUpdate := False;
  end;
end;

function TDBAdvMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBAdvMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvMemo.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvMemo.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TDBAdvMemo.DoUpdate;
begin
  EditCanModify;
end;

end.
