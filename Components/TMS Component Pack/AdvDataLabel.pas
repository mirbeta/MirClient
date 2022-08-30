{***************************************************************************}
{ TAdvDataLabel component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2015                                        }
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

unit AdvDataLabel;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Controls, Windows, Messages
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}

  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with setting DataAs* under specific circumstances

type
  TDataType = (dtString, dtInteger, dtFloat, dtDateTime);

  TEllipsis = (elNone, elEnd, elPath);

  TAdvCustomDataLabel = class(TGraphicControl)
  private
    FCaptionMargin: integer;
    FCaptionAlignment: TAlignment;
    FDataAlignment: TAlignment;
    FDataFont: TFont;
    FDataIndent: integer;
    FDataEllipsis: TEllipsis;
    FCaptionEllipsis: TEllipsis;
    FSeparator: string;
    FURLUnderline: boolean;
    FURLColor: TColor;
    procedure SetCaptionMargin(const Value: integer);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetDataAlignment(const Value: TAlignment);
    procedure SetDataIndent(const Value: integer);
    procedure SetDataFont(const Value: TFont);
    procedure SetCaptionEllipsis(const Value: TEllipsis);
    procedure SetDataEllipsis(const Value: TEllipsis);
    procedure SetSeparator(const Value: string);
    procedure SetURLColor(const Value: TColor);
    procedure SetURLUnderline(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure DrawItem(Caption, Data: string; DataFont: TFont; AAlignment: TAlignment; AEllipsis: TEllipsis; ARect: TRect; Selected: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property CaptionEllipsis: TEllipsis read FCaptionEllipsis write SetCaptionEllipsis default elNone;
    property CaptionMargin: integer read FCaptionMargin write SetCaptionMargin;
    property DataAlignment: TAlignment read FDataAlignment write SetDataAlignment default taLeftJustify;
    property DataEllipsis: TEllipsis read FDataEllipsis write SetDataEllipsis default elNone;
    property DataFont: TFont read FDataFont write SetDataFont;
    property DataIndent: integer read FDataIndent write SetDataIndent default 60;
    property Separator: string read FSeparator write SetSeparator;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property URLUnderline: boolean read FURLUnderline write SetURLUnderline default true;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDataLabel = class(TAdvCustomDataLabel)
  private
    FCaption: string;
    FData: string;
    FDataFormat: string;
    FDataAsInt: integer;
    FDataAsFloat: double;
    FDataAsDateTime: TDateTime;
    FDataType: TDataType;
    FDataFloatFormat: string;
    FDataDateFormat: string;
    FDataIntFormat: string;
    FSelected: boolean;
    FOldCursor: TCursor;
    FOnAnchorClick: TNotifyEvent;
    FDataHint: string;
    FDataURL: boolean;
    FInData: boolean;
    procedure SetCaption(const Value: string);
    procedure SetData(const Value: string);
    procedure SetDataAsDate(const Value: TDateTime);
    procedure SetDataAsDateTime(const Value: TDateTime);
    procedure SetDataAsFloat(const Value: double);
    procedure SetDataAsInt(const Value: integer);
    procedure SetDataAsTime(const Value: TDateTime);
    procedure SetDataFormat(const Value: string);
    procedure SetDataDateFormat(const Value: string);
    procedure SetDataFloatFormat(const Value: string);
    procedure SetDataIntFormat(const Value: string);
    procedure SetSelected(const Value: boolean);
    procedure SetDataURL(const Value: boolean);
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
  protected
    procedure DblClick; override;
    procedure Click; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetDataRect: TRect;
    function GetDataString: string; virtual;
    function GetCaptionString: string; virtual;
    procedure DoAnchorClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataAsInt: integer read FDataAsInt write SetDataAsInt;
    property DataAsFloat: double read FDataAsFloat write SetDataAsFloat;
    property DataAsDate: TDateTime read FDataAsDateTime write SetDataAsDate;
    property DataAsTime: TDateTime read FDataAsDateTime write SetDataAsTime;
    property DataAsDateTime: TDateTime read FDataAsDateTime write SetDataAsDateTime;
    property Selected: boolean read FSelected write SetSelected;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Caption: string read FCaption write SetCaption;
    property CaptionAlignment;
    property CaptionEllipsis;
    property Data: string read FData write SetData;
    property Font;
    property DataAlignment;
    property DataEllipsis;
    property DataFont;
    property DataFormat: string read FDataFormat write SetDataFormat;
    property DataIndent;
    property DataIntFormat: string read FDataIntFormat write SetDataIntFormat;
    property DataFloatFormat: string read FDataFloatFormat write SetDataFloatFormat;
    property DataDateFormat: string read FDataDateFormat write SetDataDateFormat;
    property DataHint: string read FDataHint write FDataHint;
    property DataURL: boolean read FDataURL write SetDataURL default false;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;

    property PopupMenu;
    property Separator;
    property ShowHint;

    property URLColor;
    property URLUnderline;
    property Version;

    property OnAnchorClick: TNotifyEvent read FOnAnchorClick write FOnAnchorClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    property OnGesture;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2007_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TAdvDataList = class;

  TDataItem = class(TCollectionItem)
  private
    FCaption: string;
    FData: string;
    FDataFormat: string;
    FDataAsInt: integer;
    FDataAsFloat: double;
    FDataAsDateTime: TDateTime;
    FDataType: TDataType;
    FDataFloatFormat: string;
    FDataDateFormat: string;
    FDataIntFormat: string;
    FDataHint: string;
    FDataURL: boolean;
    FDataFont: TFont;
    FDataEllipsis: TEllipsis;
    FDataAlignment: TAlignment;
    procedure SetCaption(const Value: string);
    procedure SetData(const Value: string);
    procedure SetDataAsDate(const Value: TDateTime);
    procedure SetDataAsDateTime(const Value: TDateTime);
    procedure SetDataAsFloat(const Value: double);
    procedure SetDataAsInt(const Value: integer);
    procedure SetDataAsTime(const Value: TDateTime);
    procedure SetDataDateFormat(const Value: string);
    procedure SetDataFloatFormat(const Value: string);
    procedure SetDataFormat(const Value: string);
    procedure SetDataIntFormat(const Value: string);
    procedure SetDataURL(const Value: boolean);
    procedure SetDataFont(const Value: TFont);
    procedure SetDataEllipsis(const Value: TEllipsis);
    procedure SetDataAlignment(const Value: TAlignment);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed;
    function GetCaptionString: string;
    function GetDataString: string;
    property DataAsInt: integer read FDataAsInt write SetDataAsInt;
    property DataAsFloat: double read FDataAsFloat write SetDataAsFloat;
    property DataAsDate: TDateTime read FDataAsDateTime write SetDataAsDate;
    property DataAsTime: TDateTime read FDataAsDateTime write SetDataAsTime;
    property DataAsDateTime: TDateTime read FDataAsDateTime write SetDataAsDateTime;
  published
    property Caption: string read FCaption write SetCaption;
    property Data: string read FData write SetData;
    property DataAlignment: TAlignment read FDataAlignment write SetDataAlignment default taLeftJustify;
    property DataEllipsis: TEllipsis read FDataEllipsis write SetDataEllipsis default elNone;
    property DataFont: TFont read FDataFont write SetDataFont;
    property DataFormat: string read FDataFormat write SetDataFormat;
    property DataIntFormat: string read FDataIntFormat write SetDataIntFormat;
    property DataFloatFormat: string read FDataFloatFormat write SetDataFloatFormat;
    property DataDateFormat: string read FDataDateFormat write SetDataDateFormat;
    property DataHint: string read FDataHint write FDataHint;
    property DataURL: boolean read FDataURL write SetDataURL default false;
  end;

  TDataCollection = class(TCollection)
  private
    FList: TAdvDataList;
    function GetItem(Index: integer): TDataItem;
    procedure SetItem(Index: integer; const Value: TDataItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TAdvDataList);
    function Add: TDataItem;
    function Insert(Index: integer): TDataItem;
    property Items[Index: integer]: TDataItem read GetItem write SetItem; default;
  end;

  TAnchorEvent = procedure(Sender: TObject; Index: integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDataList = class(TAdvCustomDataLabel)
  private
    FData: TDataCollection;
    FHintItem: integer;
    FOldCursor: TCursor;
    FOnAnchorClick: TAnchorEvent;
    procedure SetData(const Value: TDataCollection);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoAnchorClick(Index: integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function XYToItem(X,Y: integer): integer;
    function GetItemRect(Index: integer): TRect;
    function GetDataRect(Index: integer): TRect;
  published
    property Align;
    property Anchors;
    property Constraints;

    property CaptionAlignment;
    property CaptionEllipsis;

    property Data: TDataCollection read FData write SetData;

    property DataFont;
    property DataIndent;

    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;

    property Font;

    property PopupMenu;
    property Separator;
    property ShowHint;
    property Version;
    property OnAnchorClick: TAnchorEvent read FOnAnchorClick write FOnAnchorClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    property Touch;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2007_LVL}
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
  SysUtils, Forms, Math;

{$IFDEF DELPHI_UNICODE}
type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
{$ENDIF}

function AlignStyle(AAlign: TAlignment): DWORD;
begin
  Result := DT_LEFT;

  case AAlign of
  taRightJustify: Result := DT_RIGHT;
  taCenter: Result := DT_CENTER;
  end;
end;

function EllipsisStyle(AEllips: TEllipsis): DWORD;
begin
  Result := 0;

  case AEllips of
  elEnd: Result := DT_END_ELLIPSIS;
  elPath: Result := DT_PATH_ELLIPSIS;
  end;
end;



{ TAdvCustomDataLabel }

procedure TAdvCustomDataLabel.SetCaptionAlignment(const Value: TAlignment);
begin
  if (FCaptionAlignment <> Value) then
  begin
    FCaptionAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetCaptionEllipsis(const Value: TEllipsis);
begin
  if (FCaptionEllipsis <> Value) then
  begin
    FCaptionEllipsis := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetCaptionMargin(const Value: integer);
begin
  if (FCaptionMargin <> Value) then
  begin
    FCaptionMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetDataAlignment(const Value: TAlignment);
begin
  if (FDataAlignment <> Value) then
  begin
    FDataAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetDataEllipsis(const Value: TEllipsis);
begin
  if (FDataEllipsis <> Value) then
  begin
    FDataEllipsis := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetDataFont(const Value: TFont);
begin
  FDataFont.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomDataLabel.SetVersion(const Value: string);
begin

end;

procedure TAdvCustomDataLabel.SetURLColor(const Value: TColor);
begin
  if (FURLColor <> Value) then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetURLUnderline(const Value: boolean);
begin
  if (FURLUnderline <> Value) then
  begin
    FURLUnderline := Value;
    Invalidate;
  end;
end;


procedure TAdvCustomDataLabel.SetSeparator(const Value: string);
begin
  if (FSeparator <> Value) then
  begin
    FSeparator := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDataLabel.SetDataIndent(const Value: integer);
begin
  if (FDataIndent <> Value) then
  begin
    FDataIndent := Value;
    Invalidate;
  end;
end;


constructor TAdvCustomDataLabel.Create(AOwner: TComponent);
begin
  inherited;
  FSeparator := ':';
  FDataIndent := 60;
  FURLColor := clBlue;
  FURLUnderline := true;
  FDataFont := TFont.Create;
end;

destructor TAdvCustomDataLabel.Destroy;
begin
  FDataFont.Free;
  inherited;
end;

procedure TAdvCustomDataLabel.DrawItem(Caption, Data: string; DataFont: TFont; AAlignment: TAlignment; AEllipsis: TEllipsis; ARect: TRect; Selected: boolean);
var
  dstr: string;
  cstr: string;
  cr,dr: TRect;
  dstyle: DWORD;

begin
  Canvas.Brush.Style := bsClear;

  cr := ARect;

  dstr := Data;

  cstr := Caption;

  if DataIndent > 0 then
    cr.Right := DataIndent;

  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;

  dstyle := AlignStyle(CaptionAlignment) + EllipsisStyle(CaptionEllipsis);

  dstyle := dstyle or DT_SINGLELINE or DT_VCENTER;

  if Enabled then
  begin
    DrawText(Canvas.Handle,PChar(cstr),Length(cstr),cr, dstyle);
  end
  else
  begin
    Canvas.Font.Color := clWhite;
    OffsetRect(cr,1,1);
    DrawText(Canvas.Handle,PChar(cstr),Length(cstr),cr, dstyle);
    Canvas.Font.Color := clGray;
    OffsetRect(cr,-1,-1);
    DrawText(Canvas.Handle,PChar(cstr),Length(cstr),cr, dstyle);
  end;

  dr := ARect;

  if DataIndent = 0 then
  begin
    dstyle := dstyle or DT_CALCRECT;
    DrawText(Canvas.Handle,PChar(cstr),Length(cstr),cr, dstyle);
    dr.Left := cr.Right;
  end
  else
    dr.Left := DataIndent;

  Canvas.Font.Assign(DataFont);

  //if DataURL then
  //begin
  //  Canvas.Font.Color := URLColor;
  //  if URLUnderline then
  //    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
  //end;


  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
    Canvas.Brush.Style := bsSolid;
  end;

  dstyle := AlignStyle(AAlignment) + EllipsisStyle(AEllipsis);
  dstyle := dstyle or DT_SINGLELINE or DT_VCENTER;

  if Enabled then
  begin
    DrawText(Canvas.Handle,PChar(dstr),Length(dstr),dr, dstyle)
  end
  else
  begin
    Canvas.Font.Color := clWhite;
    OffsetRect(cr,1,1);
    DrawText(Canvas.Handle,PChar(dstr),Length(dstr),dr, dstyle);
    Canvas.Font.Color := clGray;
    OffsetRect(cr,-1,-1);
    DrawText(Canvas.Handle,PChar(dstr),Length(dstr),dr, dstyle);
  end;
end;

function TAdvCustomDataLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomDataLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;



{ TAdvDataLabel }

procedure TAdvDataLabel.Click;
begin
  inherited;
  Selected := false;
end;

procedure TAdvDataLabel.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  R: TRect;
begin
  inherited;
  hi := PHintInfo(Msg.LParam);
  R := GetDataRect;
  if PtInRect(R, hi^.CursorPos) and (DataHint <> '') and ShowHint then
  begin
    hi^.HintStr := DataHint;
  end;
end;

constructor TAdvDataLabel.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  FDataType := dtString;
  FDataFormat := '%s';
  FDataIntFormat := '%d';
  FDataFloatFormat := '%f';
  {$IFDEF DELPHIXE_LVL}
  FDataDateFormat := FormatSettings.ShortDateFormat;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  FDataDateFormat := ShortDateFormat;
  {$ENDIF}
  FSeparator := ':';
  Height := 13;
  Width := 150;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    Caption := 'Caption';
    Data := 'Data';
  end;

  FURLColor := clBlue;
  FURLUnderline := true;
end;

procedure TAdvDataLabel.DblClick;
begin
  inherited;
  Selected := true;
end;

destructor TAdvDataLabel.Destroy;
begin
  inherited;
end;

procedure TAdvDataLabel.DoAnchorClick;
begin
  if Assigned(OnAnchorClick) then
    OnAnchorClick(Self);
end;

function TAdvDataLabel.GetCaptionString: string;
begin
  Result := '';
  if Caption <> '' then
    Result := Caption + Separator;
end;

function TAdvDataLabel.GetDataRect: TRect;
var
  dy: integer;
  dstyle: DWORD;
  r: TRect;
  str: string;

begin
  r := ClientRect;
  Result := R;
  dstyle := DT_SINGLELINE or DT_VCENTER or DT_CALCRECT or DT_LEFT;

  if DataIndent = 0 then
  begin
    Canvas.Font.Assign(Font);
    str := GetCaptionString;
    DrawText(Canvas.Handle,PChar(str),Length(str),R,dstyle);
    Result.Left := R.Right + 4;
  end
  else
    Result.Left := DataIndent;

  str := GetDataString;
  Canvas.Font.Assign(DataFont);
  dy := DrawText(Canvas.Handle,PChar(str),Length(str),R,dstyle);
  Result.Right := Result.Left + (R.Right - R.Left);

  Result.Top := Result.Top + (Result.Bottom - Result.Top - dy) div 2;
  Result.Bottom := Result.Top + dy;
end;

function TAdvDataLabel.GetDataString: string;
begin
  Result := '';
  case FDataType of
    dtString: Result := Format(DataFormat,[Data]);
    dtInteger: Result := Format(DataIntFormat,[FDataAsInt]);
    dtFloat: Result := Format(DataFloatFormat,[FDataAsFloat]);
    dtDateTime: Result := FormatDateTime(DataDateFormat,FDataAsDateTime);
  end;
end;


procedure TAdvDataLabel.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

procedure TAdvDataLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
begin
  inherited;

  if DataURL then
  begin
    R := GetDataRect;
    if PtInRect(R,Point(X,Y)) then
      DoAnchorClick;
  end;
end;

procedure TAdvDataLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;

  if DataURL then
  begin
    R := GetDataRect;
    if PtInRect(R,Point(X,Y)) then
    begin
      if not FInData and ShowHint then
        Application.CancelHint;

      Cursor := crHandPoint;
      FInData := true;
    end
    else
    begin
      if FInData and ShowHint then
        Application.CancelHint;

      FInData := false;
      Cursor := FOldCursor;
    end;
  end;
end;

procedure TAdvDataLabel.Paint;
var
  cr: TRect;
  df: TFont;
begin

  cr := ClientRect;

  df := TFont.Create;

  try
    df.Assign(DataFont);
    if DataURL then
    begin
      df.Color := URLColor;
      if URLUnderline then
        df.Style := df.Style + [fsUnderline];
    end;

    DrawItem(GetCaptionString, GetDataString, df, DataAlignment, DataEllipsis, cr, FSelected);
  finally
    df.Free;
  end;
end;

procedure TAdvDataLabel.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetData(const Value: string);
begin
  if (FData <> Value) then
  begin
    FData := Value;
    FDataType := dtString;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataAsDate(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataAsDateTime(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) or (FDataType <> dtDateTime) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataAsFloat(const Value: double);
begin
  if (FDataAsFloat <> Value) or (FDataType <> dtFloat) then
  begin
    FDataAsFloat := Value;
    FData := '';
    FDataType := dtFloat;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataAsInt(const Value: integer);
begin
  if (FDataAsInt <> Value) or (FDataType <> dtInteger) then
  begin
    FDataAsInt := Value;
    FData := '';
    FDataType := dtInteger;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataAsTime(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Invalidate;
  end;
end;

procedure TAdvDataLabel.SetDataDateFormat(const Value: string);
begin
  if (FDataDateFormat <> Value) then
  begin
    FDataDateFormat := Value;
    Invalidate;
  end;
end;


procedure TAdvDataLabel.SetDataFloatFormat(const Value: string);
begin
  if (FDataFloatFormat <> Value) then
  begin
    FDataFloatFormat := Value;
    Invalidate;
  end;
end;


procedure TAdvDataLabel.SetDataFormat(const Value: string);
begin
  if (FDataFormat <> Value) then
  begin
    FDataFormat := Value;
    Invalidate;
  end;
end;


procedure TAdvDataLabel.SetDataIntFormat(const Value: string);
begin
  if (FDataIntFormat <> Value) then
  begin
    FDataIntFormat := Value;
    Invalidate;
  end;
end;


procedure TAdvDataLabel.SetDataURL(const Value: boolean);
begin
  if (FDataURL <> Value) then
  begin
    FDataURL := Value;
    Invalidate;
  end;
end;


procedure TAdvDataLabel.SetSelected(const Value: boolean);
begin
  if (FSelected <> Value) then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;


{ TAdvDataList }

procedure TAdvDataList.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  i: integer;
begin
  inherited;

  hi := PHintInfo(Msg.LParam);

  for i := 0 to Data.Count - 1 do
  begin
    if PtInRect(GetDataRect(i),hi^.CursorPos) then
    begin
      if Data[i].DataHint <> '' then
        hi^.HintStr := Data[i].DataHint;
    end;
  end;
end;


constructor TAdvDataList.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
  FItem: TDataItem;
begin
  inherited;
  FData := TDataCollection.Create(Self);

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FItem := FData.Add;
    FItem.Caption := 'Caption 1';
    FItem.Data := 'Data 1';
    FItem := FData.Add;
    FItem.Caption := 'Caption 2';
    FItem.Data := 'Data 2';
  end;
end;

destructor TAdvDataList.Destroy;
begin
  FData.Free;
  inherited;
end;


procedure TAdvDataList.DoAnchorClick(Index: integer);
begin
  if Assigned(OnAnchorClick) then
    OnAnchorClick(Self, Index);
end;


procedure TAdvDataList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: integer;
begin
  inherited;

  for i := 0 to Data.Count - 1 do
  begin
    if PtInRect(GetDataRect(i),Point(X,Y)) then
    begin
      if Data[i].DataURL then
      begin
        DoAnchorClick(i);
      end;
      break;
    end;
  end;
end;


procedure TAdvDataList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i,j: integer;

begin
  inherited;

  j := -1;

  for i := 0 to Data.Count - 1 do
  begin
    if PtInRect(GetDataRect(i),Point(X,Y)) then
    begin
      if ShowHint and (i <> FHintItem) then
        Application.CancelHint;

      FHintItem := i;

      if Data[i].DataURL then
      begin
        Cursor := crHandPoint;
        j := i;
      end;
      break;
    end;
  end;

  if (j = -1) and (Cursor <> FOldCursor) then
    Cursor := FOldCursor;
end;

procedure TAdvDataList.Paint;
var
  i,th,lh: integer;
  cr: TRect;
  df: TFont;
begin
  inherited;

  cr := ClientRect;

  Canvas.Font.Assign(Font);
  th := Canvas.TextHeight('gh');

  df := TFont.Create;

  try
    for i := 0 to Data.Count - 1 do
    begin
      Canvas.Font.Assign(Data.Items[i].DataFont);

      df.Assign(Data.Items[i].DataFont);

      if Data[i].DataURL then
      begin
        df.Color := URLColor;
        if URLUnderline then
          df.Style := df.Style + [fsUnderline];
      end;

      lh := Max(th, Canvas.TextHeight('gh'));
      cr.Bottom := cr.Top + lh + 4;

      DrawItem(Data[i].GetCaptionString, Data[i].GetDataString, df, Data[i].DataAlignment, Data[i].DataEllipsis, cr, false);
      OffsetRect(cr, 0, lh + 4);
    end;

  finally
    df.Free;
  end;
end;


procedure TAdvDataList.SetData(const Value: TDataCollection);
begin
  FData.Assign(Value);
  Invalidate;
end;


function TAdvDataList.XYToItem(X, Y: integer): integer;
var
  cr: TRect;
  i,th,lh: integer;
begin
  Result := -1;

  cr := ClientRect;

  Canvas.Font.Assign(Font);
  th := Canvas.TextHeight('gh');

  for i := 0 to Data.Count - 1 do
  begin
    Canvas.Font.Assign(Data.Items[i].DataFont);

    lh := Max(th, Canvas.TextHeight('gh'));
    cr.Bottom := cr.Top + lh + 4;

    if (y >= cr.Top) and (y < cr.Bottom) then
    begin
      Result := i;
      Break;
    end;
    OffsetRect(cr, 0, lh + 4);
  end;
end;

function TAdvDataList.GetItemRect(Index: integer): TRect;
var
  cr: TRect;
  i,th,lh: integer;
begin
  if (Index < 0) or (Index >= Data.Count) then
    raise Exception.Create('Data item index out of bounds');

  Result := ClientRect;

  cr := ClientRect;

  Canvas.Font.Assign(Font);
  th := Canvas.TextHeight('gh');

  for i := 0 to Index do
  begin
    Canvas.Font.Assign(Data.Items[i].DataFont);

    lh := Max(th, Canvas.TextHeight('gh'));
    cr.Bottom := cr.Top + lh + 4;

    if i < Index then
      OffsetRect(cr, 0, lh + 4)
    else
      Result := cr;
  end;
end;

procedure TAdvDataList.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

function TAdvDataList.GetDataRect(Index: Integer): TRect;
var
  cr: TRect;
  dstyle: DWORD;
  r: TRect;
  str: string;

begin
  if (Index < 0) or (Index >= Data.Count) then
    raise Exception.Create('Data item index out of bounds');

  cr := GetItemRect(Index);

  Result := cr;

  dstyle := DT_SINGLELINE or DT_VCENTER or DT_CALCRECT or DT_LEFT;

  if DataIndent = 0 then
  begin
    Canvas.Font.Assign(Font);
    str := Data[Index].GetCaptionString;
    DrawText(Canvas.Handle,PChar(str),Length(str),R,dstyle);
    Result.Left := R.Right + 4;
  end
  else
    Result.Left := DataIndent;

  str := Data[Index].GetDataString;
  Canvas.Font.Assign(Data[Index].DataFont);
  DrawText(Canvas.Handle,PChar(str),Length(str),R,dstyle);

  Result.Right := Result.Left + (R.Right - R.Left);
end;


{ TDataItem }

procedure TDataItem.Assign(Source: TPersistent);
begin
  if (Source is TDataItem) then
  begin
    FDataAsInt := (Source as TDataItem).DataAsInt;
    FDataAsFloat := (Source as TDataItem).DataAsFloat;
    FDataAsDateTime := (Source as TDataItem).DataAsDateTime;
    FCaption := (Source as TDataItem).Caption;
    FData := (Source as TDataItem).Data;
    FDataFont.Assign((Source as TDataItem).DataFont);
    FDataFormat := (Source as TDataItem).DataFormat;
    FDataIntFormat := (Source as TDataItem).DataIntFormat;
    FDataFloatFormat := (Source as TDataItem).DataFloatFormat;
    FDataDateFormat := (Source as TDataItem).DataDateFormat;
    FDataHint := (Source as TDataItem).DataHint;
    FDataURL := (Source as TDataItem).DataURL;
    FDataEllipsis := (Source as TDataItem).DataEllipsis;
    FDataAlignment := (Source as TDataItem).DataAlignment;
  end;
end;

procedure TDataItem.Changed;
begin
  (Collection as TDataCollection).Update(Self);
end;

constructor TDataItem.Create(Collection: TCollection);
begin
  inherited;
  FDataFont := TFont.Create;
  if Assigned((Collection as TDataCollection).FList.DataFont) then
    FDataFont.Assign((Collection as TDataCollection).FList.DataFont);
  FDataFormat := '%s';
  FDataIntFormat := '%d';
  FDataFloatFormat := '%f';
end;

destructor TDataItem.Destroy;
begin
  FDataFont.Free;
  inherited;
end;

function TDataItem.GetCaptionString: string;
begin
  Result := '';
  if Caption <> '' then
  begin
    Result := Caption + (Collection as TDataCollection).FList.Separator;
  end;
end;

function TDataItem.GetDataString: string;
begin
  Result := '';
  case FDataType of
    dtString: Result := Format(DataFormat,[Data]);
    dtInteger: Result := Format(DataIntFormat,[FDataAsInt]);
    dtFloat: Result := Format(DataFloatFormat,[FDataAsFloat]);
    dtDateTime: Result := FormatDateTime(DataDateFormat,FDataAsDateTime);
  end;
end;

procedure TDataItem.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TDataItem.SetData(const Value: string);
begin
  if (FData <> Value) then
  begin
    FData := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataAlignment(const Value: TAlignment);
begin
  if (FDataAlignment <> Value) then
  begin
    FDataAlignment := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataAsDate(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Changed;
  end;
end;

procedure TDataItem.SetDataAsDateTime(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Changed;
  end;
end;

procedure TDataItem.SetDataAsFloat(const Value: double);
begin
  if (FDataAsFloat <> Value) then
  begin
    FDataAsFloat := Value;
    FData := '';
    FDataType := dtFloat;
    Changed;
  end;
end;

procedure TDataItem.SetDataAsInt(const Value: integer);
begin
  if (FDataAsInt <> Value) then
  begin
    FDataAsInt := Value;
    FData := '';
    FDataType := dtInteger;
    Changed;
  end;
end;

procedure TDataItem.SetDataAsTime(const Value: TDateTime);
begin
  if (FDataAsDateTime <> Value) then
  begin
    FDataAsDateTime := Value;
    FData := '';
    FDataType := dtDateTime;
    Changed;
  end;
end;

procedure TDataItem.SetDataDateFormat(const Value: string);
begin
  if (FDataDateFormat <> Value) then
  begin
    FDataDateFormat := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataEllipsis(const Value: TEllipsis);
begin
  if (FDataEllipsis <> Value) then
  begin
    FDataEllipsis := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataFloatFormat(const Value: string);
begin
  if (FDataFloatFormat <> Value) then
  begin
    FDataFloatFormat := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataFont(const Value: TFont);
begin
  FDataFont.Assign(Value);
  Changed;
end;

procedure TDataItem.SetDataFormat(const Value: string);
begin
  if (FDataFormat <> Value) then
  begin
    FDataFormat := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataIntFormat(const Value: string);
begin
  if (FDataIntFormat <> Value) then
  begin
    FDataIntFormat := Value;
    Changed;
  end;
end;

procedure TDataItem.SetDataURL(const Value: boolean);
begin
  if (FDataURL <> Value) then
  begin
    FDataURL := Value;
    Changed;
  end;
end;


{ TDataCollection }

function TDataCollection.Add: TDataItem;
begin
  Result := TDataItem(inherited Add);
end;

constructor TDataCollection.Create(AOwner: TAdvDataList);
begin
  inherited Create(TDataItem);
  FList := AOwner;
end;

function TDataCollection.GetItem(Index: integer): TDataItem;
begin
  Result := TDataItem(inherited Items[Index]);
end;

function TDataCollection.Insert(Index: integer): TDataItem;
begin
  Result := TDataItem(inherited Insert(Index));
end;

procedure TDataCollection.SetItem(Index: integer; const Value: TDataItem);
begin
  inherited Items[Index] := Value;
end;

procedure TDataCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FList) then
    FList.Invalidate;
end;

end.
