{***************************************************************************}
{ TDBInspectorBar component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2015                                        }
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

unit DBInspectorBar;

{$I TMSDEFS.INC}

interface

uses
  InspectorBar, Classes, DB, DBCtrls, Messages, Windows, Controls, Graphics,
  SysUtils, ExtCtrls, Jpeg
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type

  TDBLookupInspectorEditLink = class;

  TDBInspectorItem = class(TInspectorItem)
  private
    FDataLink: TFieldDataLink;
    FPictureField: Boolean;
    function GetDataField: string;
    procedure SetDataField(const Value: string);
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetPictureField(const Value: Boolean);
  protected
    procedure EditStart; override;
    procedure EditStop; override;
    procedure EditChange; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Update; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property PictureField: Boolean read FPictureField write SetPictureField;
  end;

  TDBInspectorItems = class(TInspectorItems)
  private
    function GetItem(Index: Integer): TDBInspectorItem;
    procedure SetItem(Index: Integer; const Value: TDBInspectorItem);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TDBInspectorItem;
    function Insert(index: Integer): TDBInspectorItem;
    property Items[Index: Integer]: TDBInspectorItem read GetItem write SetItem; default;
  published
  end;

  TNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
                  nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh);
                  
  TButtonSet = set of TNavigateBtn;

  TButtonStyle = (bsDefault, bsGlyphs);

  TDBInspectorNavigator = class(TPersistent)
  private
    FVisibleButtons: TButtonSet;
    FOnChange: TNotifyEvent;
    FHoverButtonColor: TColor;
    FHoverButtonDownColor: TColor;
    FHints: TStringList;
    FButtonStyle: TButtonStyle;
    FButtonSize: Integer;
    procedure SetVisible(const Value: TButtonSet);
    procedure SetHints(const Value: TStringList);
    procedure SetButtonStyle(const Value: TButtonStyle);
    procedure SetButtonSize(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ButtonSize: Integer read FButtonSize write SetButtonSize;
    property ButtonStyle: TButtonStyle read FButtonStyle write SetButtonStyle;
    property Hints: TStringList read FHints write SetHints;
    property HoverButtonColor: TColor read FHoverButtonColor
      write FHoverButtonColor default clNone;
    property HoverButtonDownColor: TColor read FHoverButtonDownColor
      write FHoverButtonDownColor default clNone;
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
  end;

  TDBInspectorPanel = class;

  TInspectorDataLink = class(TDataLink)
  private
    FPanel: TDBInspectorPanel;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure EditingChanged; override;
  public
    constructor Create(APanel: TDBInspectorPanel);
    destructor Destroy; override;
  end;

  TDBInspectorPanel = class(TInspectorPanel)
  private
    FDataLink: TInspectorDataLink;
    FDataSource: TDataSource;
    FAllFields: Boolean;
    FShowNavigator: Boolean;
    FBtnFrwd: Boolean;
    FBtnPost: Boolean;
    FBtnInsDel: Boolean;
    FBtnBack: Boolean;
    FBtnEdit: Boolean;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetAllFields(const Value: Boolean);
    procedure SetShowNavigator(const Value: Boolean);
  protected
    function BackEnabled: Boolean;
    function FrwdEnabled: Boolean;
    function EditEnabled: Boolean;
    function PostEnabled: Boolean;
    function InsDelEnabled: Boolean;
    property BtnBack: Boolean read FBtnBack write FBtnBack;
    property BtnFrwd: Boolean read FBtnFrwd write FBtnFrwd;
    property BtnEdit: Boolean read FBtnEdit write FBtnEdit;
    property BtnPost: Boolean read FBtnPost write FBtnPost;
    property BtnInsDel: Boolean read FBtnInsDel write FBtnInsDel;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CreateItems: TInspectorItems; override;
  published
    property AllFields: Boolean read FAllFields write SetAllFields;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property ShowNavigator: Boolean read FShowNavigator write SetShowNavigator;
  end;

  TDBInspectorPanels = class(TInspectorPanels)
  private
    function GetItem(Index: Integer): TDBInspectorPanel;
    procedure SetItem(Index: Integer; const Value: TDBInspectorPanel);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TDBInspectorPanel;
    function Insert(index: Integer): TDBInspectorPanel;
    property Items[Index: Integer]: TDBInspectorPanel read GetItem write SetItem; default;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBInspectorBar = class(TInspectorBar)
  private
    FDBLookupIL: TDBLookupInspectorEditLink;
    FNavigator: TDBInspectorNavigator;
    FShowMemoFields: Boolean;
    FShowPictureFields: Boolean;
    FRepeatTimer: TTimer;
    FInitRepeatPause: Integer;
    FRepeatPause: Integer;
    FRepeatBtn: Integer;
    FRepeatBtnRect: TRect;
    FRepeatDBPanel: TDBInspectorPanel;
    procedure TimerExpired(Sender: TObject);
    function GetPanels: TDBInspectorPanels;
    procedure SetPanels(const Value: TDBInspectorPanels);
    procedure SetShowMemoFields(const Value: Boolean);
    procedure RefreshItemsValue;
    procedure SetShowPictureFields(const Value: Boolean);
  protected
    function NaviWidth: Integer;
    procedure DrawCaptionOptions(Panel: TInspectorPanel; Canvas: TCanvas;
      var R: TRect); override;
    function MouseDownCaptionOptions(Panel: TInspectorPanel;
      x,y: Integer): Boolean; override;
    function MouseMoveCaptionOptions(Panel: TInspectorPanel;
      x,y: Integer): Boolean; override;
    function HintCaptionOptions(Panel: TInspectorPanel; x,y: Integer;
      var Hint: string): Boolean; override;
    procedure NavigatorChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePanels: TInspectorPanels; override;
    procedure StartEdit(InspectorItem: TInspectorItem); override;
    procedure StopEdit(InspectorItem: TInspectorItem); override;
    procedure GetValueList(InspectorItem: TInspectorItem; Values: TStringList); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    property Navigator: TDBInspectorNavigator read FNavigator write FNavigator;
    property Panels: TDBInspectorPanels read GetPanels write SetPanels;
    property ShowMemoFields: Boolean read FShowMemoFields write SetShowMemoFields;
    property ShowPictureFields: Boolean read FShowPictureFields write SetShowPictureFields;
  end;

  TDBLookupInspectorEditLink = class(TInspectorEditLink)
  private
    FEdit: TDBLookupComboBox;
    FItem: TDBInspectorItem;
  protected
    procedure LookupChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetEditor: TWinControl; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    procedure SetProperties(R: TRect; Item: TInspectorItem); override;
    procedure SetOriginalValue; override;
    procedure StartEdit(Item: TInspectorItem); override;
    procedure StopEdit(Item: TInspectorItem); override;
  published
  end;

implementation

{ TDBInspectorBar }

{$R DBInspectorBar.res}
{$R InspGlyphs.res}

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

constructor TDBInspectorBar.Create(AOwner: TComponent);
begin
  inherited;
  FNavigator := TDBInspectorNavigator.Create(Self);
  FNavigator.OnChange := NavigatorChanged;
  FShowMemoFields := False;
  FShowPictureFields := False;
  FInitRepeatPause := 400;
  FRepeatPause := 100;
  FRepeatBtn := -1;
  FDBLookupIL := TDBLookupInspectorEditLink.Create(Self);
end;

function TDBInspectorBar.CreatePanels: TInspectorPanels;
begin
  Result := TDBInspectorPanels.Create(Self);
end;

destructor TDBInspectorBar.Destroy;
begin
  FNavigator.Free;
  FDBLookupIL.Free;
  inherited;
end;

procedure TDBInspectorBar.DrawCaptionOptions(Panel: TInspectorPanel; Canvas: TCanvas;
  var R: TRect);
var
  DrwRect,BmpRect: TRect;
  i: TNavigateBtn;
  bmp: TBitmap;
  pt: TPoint;
  BtnEnabled, Down: Boolean;
  DBPanel: TDBInspectorPanel;

begin
  inherited;

  DBPanel := TDBInspectorPanel(Panel);

  if not DBPanel.ShowNavigator then
    Exit;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  DrwRect.Left := R.Right - NaviWidth;

  R.Right := DrwRect.Left;

  for I := nbFirst to nbRefresh do
  begin
    if I in Navigator.VisibleButtons then
    begin
      bmp := TBitmap.Create;

      try
        case I of
        nbFirst: BtnEnabled := DBPanel.BackEnabled;
        nbPrior: BtnEnabled := DBPanel.BackEnabled;
        nbNext: BtnEnabled := DBPanel.FrwdEnabled;
        nbLast: BtnEnabled := DBPanel.FrwdEnabled;
        nbInsert: BtnEnabled := DBPanel.InsDelEnabled;
        nbDelete: BtnEnabled := DBPanel.InsDelEnabled;
        nbEdit: BtnEnabled := DBPanel.EditEnabled;
        nbPost: BtnEnabled := DBPanel.PostEnabled;
        nbCancel: BtnEnabled := DBPanel.PostEnabled;
        nbRefresh: BtnEnabled := DBPanel.InsDelEnabled;
        else
          BtnEnabled := False;
        end;

        if Navigator.ButtonStyle = bsDefault then
        begin
          if BtnEnabled then
            case I of
            nbFirst: bmp.LoadFromResourceName(Hinstance,'TMSNFIRST');
            nbPrior: bmp.LoadFromResourceName(Hinstance,'TMSNPRIOR');
            nbNext: bmp.LoadFromResourceName(Hinstance,'TMSNNEXT');
            nbLast: bmp.LoadFromResourceName(Hinstance,'TMSNLAST');
            nbInsert: bmp.LoadFromResourceName(Hinstance,'TMSNINSERT');
            nbDelete: bmp.LoadFromResourceName(Hinstance,'TMSNDELETE');
            nbEdit: bmp.LoadFromResourceName(Hinstance,'TMSNEDIT');
            nbPost: bmp.LoadFromResourceName(Hinstance,'TMSNPOST');
            nbCancel: bmp.LoadFromResourceName(Hinstance,'TMSNCANCEL');
            nbRefresh: bmp.LoadFromResourceName(Hinstance,'TMSNREFRESH');
            end;

          if not BtnEnabled then
            case I of
            nbFirst: bmp.LoadFromResourceName(Hinstance,'TMSNFIRSTD');
            nbPrior: bmp.LoadFromResourceName(Hinstance,'TMSNPRIORD');
            nbNext: bmp.LoadFromResourceName(Hinstance,'TMSNNEXTD');
            nbLast: bmp.LoadFromResourceName(Hinstance,'TMSNLASTD');
            nbInsert: bmp.LoadFromResourceName(Hinstance,'TMSNINSERTD');
            nbDelete: bmp.LoadFromResourceName(Hinstance,'TMSNDELETED');
            nbEdit: bmp.LoadFromResourceName(Hinstance,'TMSNEDITD');
            nbPost: bmp.LoadFromResourceName(Hinstance,'TMSNPOSTD');
            nbCancel: bmp.LoadFromResourceName(Hinstance,'TMSNCANCELD');
            nbRefresh: bmp.LoadFromResourceName(Hinstance,'TMSNREFRESHD');
            end;
        end
        else
        begin
          if BtnEnabled then
            case I of
            nbFirst: bmp.LoadFromResourceName(Hinstance,'GLYPHFIRSTE');
            nbPrior: bmp.LoadFromResourceName(Hinstance,'GLYPHPRIORE');
            nbNext: bmp.LoadFromResourceName(Hinstance,'GLYPHNEXTE');
            nbLast: bmp.LoadFromResourceName(Hinstance,'GLYPHLASTE');
            nbInsert: bmp.LoadFromResourceName(Hinstance,'GLYPHINSERTE');
            nbDelete: bmp.LoadFromResourceName(Hinstance,'GLYPHDELETEE');
            nbEdit: bmp.LoadFromResourceName(Hinstance,'GLYPHEDITE');
            nbPost: bmp.LoadFromResourceName(Hinstance,'GLYPHPOSTE');
            nbCancel: bmp.LoadFromResourceName(Hinstance,'GLYPHCANCELE');
            nbRefresh: bmp.LoadFromResourceName(Hinstance,'GLYPHREFRESHE');
            end;

          if not BtnEnabled then
            case I of
            nbFirst: bmp.LoadFromResourceName(Hinstance,'GLYPHFIRSTD');
            nbPrior: bmp.LoadFromResourceName(Hinstance,'GLYPHPRIORD');
            nbNext: bmp.LoadFromResourceName(Hinstance,'GLYPHNEXTD');
            nbLast: bmp.LoadFromResourceName(Hinstance,'GLYPHLASTD');
            nbInsert: bmp.LoadFromResourceName(Hinstance,'GLYPHINSERTD');
            nbDelete: bmp.LoadFromResourceName(Hinstance,'GLYPHDELETED');
            nbEdit: bmp.LoadFromResourceName(Hinstance,'GLYPHEDITD');
            nbPost: bmp.LoadFromResourceName(Hinstance,'GLYPHPOSTD');
            nbCancel: bmp.LoadFromResourceName(Hinstance,'GLYPHCANCELD');
            nbRefresh: bmp.LoadFromResourceName(Hinstance,'GLYPHREFRESHD');
            end;
        end;

        DrwRect.Top := R.Top + 3;
        DrwRect.Right := DrwRect.Left + bmp.Width;
        DrwRect.Bottom := DrwRect.Top + Navigator.ButtonSize;

        bmp.TransparentMode := tmAuto;
        bmp.Transparent := True;

        BmpRect := DrwRect;

        BmpRect.Left := BmpRect.Left + (Navigator.ButtonSize + 2 - bmp.Width) shr 1;
        BmpRect.Right := BmpRect.Left + bmp.Width;

        case I of
        nbFirst: BtnEnabled := DBPanel.BackEnabled;
        nbPrior: BtnEnabled := DBPanel.BackEnabled;
        nbNext: BtnEnabled := DBPanel.FrwdEnabled;
        nbLast: BtnEnabled := DBPanel.FrwdEnabled;
        nbInsert: BtnEnabled := DBPanel.InsDelEnabled;
        nbDelete: BtnEnabled := DBPanel.InsDelEnabled;
        nbEdit: BtnEnabled := DBPanel.EditEnabled;
        nbPost: BtnEnabled := DBPanel.PostEnabled;
        nbCancel: BtnEnabled := DBPanel.PostEnabled;
        nbRefresh: BtnEnabled := DBPanel.InsDelEnabled;
        else
          BtnEnabled := False;
        end;

        Down := False;

        InflateRect(BmpRect,1,1);
        if PtInRect(BmpRect,pt) and BtnEnabled then
        begin

          if Navigator.ButtonStyle <> bsDefault then
            case I of
            nbFirst: bmp.LoadFromResourceName(Hinstance,'GLYPHFIRSTH');
            nbPrior: bmp.LoadFromResourceName(Hinstance,'GLYPHPRIORH');
            nbNext: bmp.LoadFromResourceName(Hinstance,'GLYPHNEXTH');
            nbLast: bmp.LoadFromResourceName(Hinstance,'GLYPHLASTH');
            nbInsert: bmp.LoadFromResourceName(Hinstance,'GLYPHINSERTH');
            nbDelete: bmp.LoadFromResourceName(Hinstance,'GLYPHDELETEH');
            nbEdit: bmp.LoadFromResourceName(Hinstance,'GLYPHEDITH');
            nbPost: bmp.LoadFromResourceName(Hinstance,'GLYPHPOSTH');
            nbCancel: bmp.LoadFromResourceName(Hinstance,'GLYPHCANCELH');
            nbRefresh: bmp.LoadFromResourceName(Hinstance,'GLYPHREFRESHH');
            end;

          BmpRect.Bottom := BmpRect.Bottom + 1;

          Canvas.Brush.Color := clNone;

          if IsMouseDown and (Navigator.HoverButtonDownColor <> clNone) then
            Canvas.Brush.Color := Navigator.HoverButtonDownColor
          else
            if (Navigator.HoverButtonColor <> clNone) then
            Canvas.Brush.Color := Navigator.HoverButtonColor;

          if Canvas.Brush.Color <> clNone then
          begin
            Canvas.Pen.Color := clBlack;
            Canvas.Pen.Width := 1;
            Canvas.Brush.Style := bsSolid;
            Canvas.Rectangle(bmpRect.Left,BmpRect.Top,BmpRect.Right,BmpRect.Bottom);
          end
          else
          begin
            if IsMouseDown then
              Canvas.Pen.Color := clGray
            else
              Canvas.Pen.Color := clWhite;

            Canvas.MoveTo(bmpRect.Left,bmpRect.Bottom);
            Canvas.LineTo(bmpRect.Left,bmpRect.Top);
            Canvas.LineTo(bmpRect.Right,bmpRect.Top);

            if IsMouseDown then
              Canvas.Pen.Color := clWhite
            else
              Canvas.Pen.Color := clGray;

            Canvas.LineTo(bmpRect.Right,bmpRect.Bottom);
            Canvas.LineTo(bmpRect.Left,bmpRect.Bottom);
          end;

          Canvas.Brush.Style := bsClear;
          Down := IsMouseDown;
        end;

        BmpRect := DrwRect;
        BmpRect.Left := BmpRect.Left + (Navigator.ButtonSize + 2 - bmp.Width) shr 1;
        BmpRect.Right := BmpRect.Left + bmp.Width;

        if Down then
          OffsetRect(BmpRect,1,1);

        Canvas.Draw(BmpRect.Left,bmprect.top, bmp);
        {
        if BtnEnabled then
          Canvas.BrushCopy(BmpRect,bmp,Rect(0,0,(bmp.Width shr 1),12),clOlive)
        else
          Canvas.BrushCopy(BmpRect,bmp,Rect((bmp.Width shr 1),0,bmp.Width,12),clOlive);
        }
      finally
        bmp.Free;
      end;

      DrwRect.Left := DrwRect.Left + Navigator.ButtonSize + 3;
    end;
  end;
end;

function TDBInspectorBar.GetPanels: TDBInspectorPanels;
begin
  Result := TDBInspectorPanels(inherited Panels);
end;

procedure TDBInspectorBar.GetValueList(InspectorItem: TInspectorItem;
  Values: TStringList);
var
  Field: TField;
  DBPanel: TDBInspectorPanel;
  DBItem : TDBInspectorItem;
begin
  DBItem := TDBInspectorItem(InspectorItem);
  DBPanel := TDBInspectorPanel(DBItem.InspectorPanel);
  if (DBItem.DataField <> '') and
     Assigned(DBPanel.DataSource)  then
  begin
    if Assigned(DBPanel.DataSource.DataSet) then
    begin
      Field := DBPanel.DataSource.DataSet.FieldByName(DBItem.DataField);
      if Assigned(Field) then
      begin
        // fetch lookup dataset here
      end;
    end;
  end;
  inherited;
end;

function TDBInspectorBar.HintCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer; var Hint: string): Boolean;
var
  i: TNavigateBtn;
  DBPanel: TDBInspectorPanel;
  Indent: Integer;
begin
  Result := False;

  DBPanel := TDBInspectorPanel(Panel);

  if not DBPanel.ShowNavigator then
    Exit;


  Indent := HelpWidth - NaviWidth;

  Result := x > Indent;

  if not Result then
    Exit;

  for I := nbFirst to nbRefresh do
  begin
    if I in Navigator.VisibleButtons then
    begin
      if (x > Indent) and (x < Indent + Navigator.ButtonSize + 3) then
        case I of
        nbFirst: if Navigator.Hints.Count > integer(nbFirst) then
          Hint := Navigator.Hints.Strings[integer(nbFirst)];

        nbPrior: if Navigator.Hints.Count > integer(nbPrior) then
          Hint := Navigator.Hints.Strings[integer(nbPrior)];

        nbNext: if Navigator.Hints.Count > integer(nbNext) then
          Hint := Navigator.Hints.Strings[integer(nbNext)];

        nbLast: if Navigator.Hints.Count > integer(nbLast) then
          Hint := Navigator.Hints.Strings[integer(nbLast)];

        nbInsert: if Navigator.Hints.Count > integer(nbInsert) then
          Hint := Navigator.Hints.Strings[integer(nbInsert)];

        nbDelete: if Navigator.Hints.Count > integer(nbDelete) then
          Hint := Navigator.Hints.Strings[integer(nbDelete)];

        nbEdit: if Navigator.Hints.Count > integer(nbEdit) then
          Hint := Navigator.Hints.Strings[integer(nbEdit)];

        nbPost: if Navigator.Hints.Count > integer(nbPost) then
          Hint := Navigator.Hints.Strings[integer(nbPost)];

        nbCancel: if Navigator.Hints.Count > integer(nbCancel) then
          Hint := Navigator.Hints.Strings[integer(nbCancel)];

        nbRefresh: if Navigator.Hints.Count > integer(nbRefresh) then
          Hint := Navigator.Hints.Strings[integer(nbRefresh)];

        end;
      Indent := Indent + Navigator.ButtonSize + 3;
    end;
  end;

end;

function TDBInspectorBar.MouseDownCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer): Boolean;
var
  I: TNavigateBtn;
  Indent: Integer;
  DBPanel: TDBInspectorPanel;
  j, IndexV: Integer;

begin
  Result := False;

  DBPanel := TDBInspectorPanel(Panel);

  if not DBPanel.ShowNavigator then
    Exit;

  Indent := HelpWidth - NaviWidth;

  Result := x > Indent;

  if not Result then
    Exit;

  for I := nbFirst to nbRefresh do
  begin
    if I in Navigator.VisibleButtons then
    begin
      if (x > Indent) and (x < Indent + Navigator.ButtonSize + 3) then
        case I of
        nbFirst: if DBPanel.BackEnabled then
          DBPanel.DataSource.DataSet.First;
        nbPrior: if DBPanel.BackEnabled then
          begin
            DBPanel.DataSource.DataSet.MoveBy(-1);
            FRepeatBtn := 1;
            FRepeatDBPanel := DBPanel;

            IndexV := 0;
            j := 1;
            while j <= Panels.Count do
            begin
              if (Panels.Items[j - 1] = DBPanel) then
                Break;

              if Panels.Items[j - 1].Visible then
                IndexV := IndexV + PanelCaption.Height;
              inc(j);
            end;

            FRepeatBtnRect := Rect(Indent, IndexV, Indent + Navigator.ButtonSize + 3, indexV + PanelCaption.Height);
            if FRepeatTimer = nil then
              FRepeatTimer := TTimer.Create(Self);
            FRepeatTimer.Enabled  := false;
            FRepeatTimer.OnTimer := TimerExpired;
            FRepeatTimer.Interval := InitRepeatPause;
            FRepeatTimer.Enabled  := True;
          end;
        nbNext: if DBPanel.FrwdEnabled then
          begin
            DBPanel.DataSource.DataSet.MoveBy(+1);
            FRepeatBtn := 2;
            FRepeatDBPanel := DBPanel;

            IndexV := 0;
            j := 1;
            while j <= Panels.Count do
            begin
              if (Panels.Items[j - 1] = DBPanel) then
                Break;

              if Panels.Items[j - 1].Visible then
                IndexV := IndexV + PanelCaption.Height;
              inc(j);
            end;

            FRepeatBtnRect := Rect(Indent, IndexV, Indent + Navigator.ButtonSize + 3, indexV + PanelCaption.Height);
            if FRepeatTimer = nil then
              FRepeatTimer := TTimer.Create(Self);

            FRepeatTimer.Enabled  := false;
            FRepeatTimer.OnTimer := TimerExpired;
            FRepeatTimer.Interval := InitRepeatPause;
            FRepeatTimer.Enabled  := True;
          end;
        nbLast: if DBPanel.FrwdEnabled then
          DBPanel.DataSource.DataSet.Last;
        nbInsert: if DBPanel.InsDelEnabled then
          DBPanel.DataSource.DataSet.Insert;
        nbDelete: if DBPanel.InsDelEnabled then
          DBPanel.DataSource.DataSet.Delete;
        nbEdit: if DBPanel.EditEnabled then
          DBPanel.DataSource.DataSet.Edit;
        nbPost: if DBPanel.PostEnabled then
          DBPanel.DataSource.DataSet.Post;
        nbCancel: if DBPanel.PostEnabled then
          DBPanel.DataSource.DataSet.Cancel;
        nbRefresh: if DBPanel.InsDelEnabled then
          DBPanel.DataSource.DataSet.Refresh;
        end;
      Indent := Indent + Navigator.ButtonSize + 3;
    end;
  end;
end;

function TDBInspectorBar.MouseMoveCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer): Boolean;
var
  Indent: integer;
begin
  Result := TDBInspectorPanel(Panel).ShowNavigator;

  Indent := HelpWidth - NaviWidth;

  Result := Result and (x > Indent);

end;

procedure TDBInspectorBar.TimerExpired(Sender: TObject);
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  FRepeatTimer.Interval := RepeatPause;
  if MouseCapture and PtInRect(FRepeatBtnRect, pt) and (FRepeatBtn in [1, 2]) and Assigned(FRepeatDBPanel) then
  begin
    try
      case FRepeatBtn of
        1: FRepeatDBPanel.DataSource.DataSet.MoveBy(-1);
        2: FRepeatDBPanel.DataSource.DataSet.MoveBy(+1);
      end;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TDBInspectorBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);

  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TDBInspectorBar.NavigatorChanged(Sender: TObject);
begin
  Invalidate;
end;

function TDBInspectorBar.NaviWidth: Integer;
var
  I: TNavigateBtn;
begin
  Result := 0;
  for I := nbFirst to nbRefresh do
  begin
    if I in Navigator.VisibleButtons then
      Result := Result + Navigator.ButtonSize + 3;
  end;
end;

procedure TDBInspectorBar.RefreshItemsValue;
var
  i, j: Integer;
begin
  for i:= 0 to Panels.Count-1 do
  begin
    for j:= 0 to Panels[i].Items.Count-1 do
    begin
      TDBInspectorItem(Panels[i].Items[j]).DataChange(nil);
    end;
  end;
end;

procedure TDBInspectorBar.SetPanels(const Value: TDBInspectorPanels);
begin
  inherited Panels := Value;
end;

procedure TDBInspectorBar.SetShowMemoFields(const Value: Boolean);
begin
  if FShowMemoFields <> Value then
  begin
    FShowMemoFields := Value;
    RefreshItemsValue;
    Invalidate;
  end;
end;

procedure TDBInspectorBar.SetShowPictureFields(const Value: Boolean);
begin
  if FShowPictureFields <> Value then
  begin
    FShowPictureFields := Value;
    RefreshItemsValue;
    Invalidate;
  end;
end;

procedure TDBInspectorBar.StartEdit(InspectorItem: TInspectorItem);
begin
  if InspectorItem.PropertyType = ptCustom then
    InspectorItem.EditLink := FDBLookupIL;

  inherited;




end;

procedure TDBInspectorBar.StopEdit(InspectorItem: TInspectorItem);
begin
  inherited;
end;

{ TDBInspectorPanels }

function TDBInspectorPanels.Add: TDBInspectorPanel;
begin
  Result := TDBInspectorPanel(inherited Add);
end;

function TDBInspectorPanels.CreateItemClass: TCollectionItemClass;
begin
  Result := TDBInspectorPanel;
end;

function TDBInspectorPanels.GetItem(Index: Integer): TDBInspectorPanel;
begin
  Result := TDBInspectorPanel(inherited Items[Index]);
end;

function TDBInspectorPanels.Insert(index: Integer): TDBInspectorPanel;
begin
  Result := TDBInspectorPanel(inherited Insert(Index));
end;

procedure TDBInspectorPanels.SetItem(Index: Integer;
  const Value: TDBInspectorPanel);
begin
  inherited Items[Index] := Value;
end;

{ TDBInspectorPanel }


function TDBInspectorPanel.BackEnabled: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := BtnBack and not DataSource.DataSet.BOF;
end;

constructor TDBInspectorPanel.Create(Collection: TCollection);
begin
  inherited;
  Style := psProperties;
  ItemHeight := 26;
  FDataLink := TInspectorDataLink.Create(Self);

  FBtnBack := False;
  FBtnFrwd := False;
  FBtnEdit := False;
  FBtnPost := False;
  FBtnInsDel := False;
end;

function TDBInspectorPanel.CreateItems: TInspectorItems;
begin
  Result := TDBInspectorItems.Create(Self);
end;

destructor TDBInspectorPanel.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TDBInspectorPanel.EditEnabled: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := BtnEdit and DataSource.DataSet.CanModify and not (DataSource.DataSet.State = dsEdit);
end;


function TDBInspectorPanel.FrwdEnabled: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := BtnFrwd and not DataSource.DataSet.EOF;
end;

function TDBInspectorPanel.InsDelEnabled: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := BtnInsDel and DataSource.DataSet.CanModify;
end;

function TDBInspectorPanel.PostEnabled: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := BtnPost and DataSource.DataSet.CanModify;
end;

procedure TDBInspectorPanel.SetAllFields(const Value: Boolean);
var
  i: Integer;
  DBI: TDBInspectorItem;
begin
  FAllFields := Value;
  if Value and (Items.Count = 0) and Assigned(DataSource) and
    not (csLoading in InspectorBar.ComponentState) then
  begin
    if Assigned(DataSource.DataSet) then
      for i := 1 to DataSource.DataSet.Fields.Count do
      begin
        DBI := TDBInspectorItem(Items.Add);
        DBI.DataField := DataSource.DataSet.Fields[i - 1].FieldName;
        DBI.Caption := DataSource.DataSet.Fields[i - 1].DisplayName;

        case DataSource.DataSet.Fields[i - 1].DataType of
        ftSmallInt,ftInteger,ftWord,ftLargeInt: DBI.PropertyType := ptInteger;
        ftFloat: DBI.PropertyType := ptFloat;
        ftBoolean: DBI.PropertyType := ptBoolean;
        ftDate: DBI.PropertyType := ptDate;
        ftTime: DBI.PropertyType := ptTime;
        else
          begin
            if DataSource.DataSet.Fields[i - 1].FieldKind = fkLookup then
            begin
              DBI.PropertyType := ptCustom;
            end
            else
              DBI.PropertyType := ptText;
          end;
        end
      end;
  end;
end;

procedure TDBInspectorPanel.SetDataSource(const Value: TDataSource);
var
  i: Integer;
begin
  FDataSource := Value;

  for i := 1 to Items.Count do
    TDBInspectorItem(Items[i - 1]).FDataLink.DataSource := Value;

  FDataLink.DataSource := Value;
end;

procedure TDBInspectorPanel.SetShowNavigator(const Value: Boolean);
begin
  FShowNavigator := Value;
  Changed(False);
end;

{ TDBInspectorItems }

function TDBInspectorItems.Add: TDBInspectorItem;
begin
  Result := TDBInspectorItem(inherited Add);
end;

function TDBInspectorItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TDBInspectorItem;
end;

function TDBInspectorItems.GetItem(Index: Integer): TDBInspectorItem;
begin
  Result := TDBInspectorItem(inherited Items[Index]);
end;

function TDBInspectorItems.Insert(index: Integer): TDBInspectorItem;
begin
  Result := TDBInspectorItem(inherited Insert(Index));
end;

procedure TDBInspectorItems.SetItem(Index: Integer;
  const Value: TDBInspectorItem);
begin
  inherited Items[Index] := Value;
end;

{ TDBInspectorItem }

procedure TDBInspectorItem.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

constructor TDBInspectorItem.Create(Collection: TCollection);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.DataSource := TDBInspectorPanel(InspectorPanel).DataSource;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FPictureField := False;
end;

procedure TDBInspectorItem.DataChange(Sender: TObject);
var
  sp: TPoint;

  procedure BlobFieldToStream(DBField: TBlobField; var size: tpoint);
  var
    s, ms: TMemoryStream;
    sig: word;
    b: byte;
    Ljpg: TJPEGImage;
    ABitmap: TBitmap;
    APicture: TPicture;
    oletype: integer;
    oleoffset: integer;
    i: Integer;

  begin
    size.X := 0;
    size.Y := 0;

    if (TBlobField(DBField).BlobType = ftGraphic) or (1 > 0) then
    begin
      s := TMemoryStream.Create;
      try
        DBField.SaveToStream(S);

        oletype := -1;
        oleoffset := 0;

        if s.Size > 2 then
        begin
      // find file type
          S.Position := 0;
          S.Read(sig, 2);

          case sig of
            $1C15: // OLE storage
              begin
                i := 0;
                while (i < 512) do
                begin
                  S.Read(b, 1);
                  inc(i);
                  if (b = $FF) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if b = $D8 then
                    begin
                      oletype := 1;
                      oleoffset := i;
                      break;
                    end;
                  end;
                  if (b = $47) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if b = $49 then
                    begin
                      oletype := 2;
                      oleoffset := i;
                      break;
                    end;
                  end;
                  if (b = ord('B')) then
                  begin
                    S.Read(b, 1);
                    inc(i);
                    if (b = ord('M')) then
                    begin
                      oletype := 0;
                      oleoffset := i;
                      Break;
                    end;
                  end;
                end;

                S.Seek(oleoffset, 0);


                case oletype of
                  0:
                    begin
                      ABitmap := TBitmap.Create;
                      ABitmap.LoadFromStream(S);
                      if not ABitmap.Empty then
                      begin
                        PictureValue.Assign(ABitmap);

                        size.X := ABitmap.Width;
                        size.Y := ABitmap.Height;
                      end;
                      ABitmap.Free;
                    end;
                  1:
                    begin
                      LJPg := TJPEGImage.Create;
                      try
                        LJpg.LoadFromStream(S);
                        APicture := TPicture.Create;
                        APicture.Assign(LJpg);

                        if not APicture.Graphic.Empty then
                        begin
                          PictureValue.Assign(APicture);

                          size.X := APicture.Graphic.Width;
                          size.Y := APicture.Graphic.Height;
                        end;
                        APicture.Free;
                      finally
                        FreeAndNil(LJpg);
                      end;
                    end;

                  2:
                    begin
                      ms := TMemoryStream.Create;
                      ms.CopyFrom(s, s.Size - s.Position);
                      PictureValue.Graphic.LoadFromStream(ms);

                      ms.Free;
                    end;
                end;
              end;
            $4947: //gif signature
              begin
                ms := TMemoryStream.Create;
                s.Position := 0;
                ms.LoadFromStream(s);

                PictureValue.Graphic.LoadFromStream(ms);

                ms.Free;
              end;
            $D8FF: //jpeg signature
              begin
                S.Position := 0;
                LJPg := TJPEGImage.Create;
                try
                  LJpg.LoadFromStream(S);
                  APicture := TPicture.Create;
                  APicture.Assign(LJpg);

                  if not APicture.Graphic.Empty then
                  begin
                    PictureValue.Assign(APicture);

                    size.X := APicture.Graphic.Width;
                    size.Y := APicture.Graphic.Height;
                  end;
                  APicture.Free;
                finally
                  FreeAndNil(LJpg);
                end;

              end
          else
            begin
              APicture := TPicture.Create;
              APicture.Assign(DBField);
              if not APicture.Graphic.Empty then
              begin
                PictureValue.Assign(APicture);

                size.X := APicture.Graphic.Width;
                size.Y := APicture.Graphic.Height;
                APicture.Free;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(S);
      end;
    end;
  end;

begin
  if DataField = '' then
    Exit;

  if Assigned(FDataLink.Field) then
  begin
    case FDataLink.Field.DataType of
    ftSmallInt,ftInteger,ftWord:
      begin
        IntValue := FDataLink.Field.AsInteger;
        TextValue := FDataLink.Field.DisplayText;
      end;
    ftFloat:
      begin
        //FloatValue := FDataLink.Field.AsInteger;
        TextValue := FDataLink.Field.DisplayText;
      end;
    ftDate,ftTime:
      begin
        DateValue := FDataLink.Field.AsDateTime;
        TimeValue := FDataLink.Field.AsDateTime;
        DateTimeValue := FDataLink.Field.AsDateTime;
        TextValue := FDataLink.Field.DisplayText;
      end;
    ftBoolean:
      begin
        BoolValue := FDataLink.Field.AsBoolean;
        if BoolValue then
          TextValue := InspectorBar.CheckTrue
        else
          TextValue := InspectorBar.CheckFalse;
      end;
    ftMemo,ftFmtMemo:
      begin
        if TDBInspectorBar(InspectorBar).ShowMemoFields then
          TextValue := FDataLink.Field.AsString
        else
          TextValue := FDataLink.Field.DisplayText;
      end;
    ftGraphic:
      begin
        if TDBInspectorBar(InspectorBar).ShowPictureFields then
        begin
          PictureValue.Assign(FDataLink.Field);
        end
        else
          TextValue := FDataLink.Field.DisplayText;
      end;
    ftBlob:
      begin
        if TDBInspectorBar(InspectorBar).ShowPictureFields and PictureField then
          BlobFieldToStream(TBlobField(FDataLink.Field), sp) // Sets PictureValue
        else
          TextValue := FDataLink.Field.AsString;
      end;
    else
      begin
        TextValue := FDataLink.Field.AsString;
      end;
    end;
  end
  else
  begin
    TextValue := '';
  end;
end;

procedure TDBInspectorItem.DataUpdate(Sender: TObject);
begin
  if Editing then
  begin
    InspectorBar.StopEdit(Self);
  end;

  if Assigned(FDataLink.Field) then
  begin
    case FDataLink.Field.DataType of
    ftSmallInt,ftInteger,ftWord: FDataLink.Field.AsInteger := IntValue;
    ftDate:
      begin
        FDataLink.Field.AsDateTime := DateValue;
      end;
    ftTime:
      begin
        FDataLink.Field.AsDateTime := TimeValue;
      end;
    ftBoolean:
      begin
        FDataLink.Field.AsBoolean := BoolValue;
        if BoolValue then
          TextValue := InspectorBar.CheckTrue
        else
          TextValue := InspectorBar.CheckFalse;
      end;
    else
      FDataLink.Field.AsString := TextValue;
    end
  end;
end;

destructor TDBInspectorItem.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

procedure TDBInspectorItem.EditChange;
begin
  inherited;
  FDataLink.Edit;
  FDataLink.Modified;
end;

procedure TDBInspectorItem.EditStart;
begin
  inherited;
end;

procedure TDBInspectorItem.EditStop;
begin
  inherited;
  if not FDataLink.ReadOnly then
  begin
    try
      // tell data link to update database
      FDataLink.UpdateRecord;
    except
    end;
  end;
end;

function TDBInspectorItem.GetDataField: string;
begin
  Result := FDataLink.Fieldname;
end;

procedure TDBInspectorItem.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;

  if not Assigned(FDataLink.Field) then
    Exit;

  case FDataLink.Field.DataType of
  ftSmallInt,ftInteger,ftWord: PropertyType := ptIntSpin;
  ftBoolean: PropertyType := ptBoolean;
  ftDate: PropertyType := ptDate;
  ftTime: PropertyType := ptTime;
  //ftMemo,ftGraphic,ftFmtMemo,ftBlob: PropertyType := ptPropButton;
  ftMemo,ftFmtMemo:
    begin
      if TDBInspectorBar(InspectorBar).ShowMemoFields then
        PropertyType := ptText
      else
        PropertyType := ptPropButton;
    end;
  ftGraphic: PropertyType := ptPicture;
  ftBlob:
    begin
      if PictureField then
        PropertyType := ptPicture
      else
        PropertyType := ptPropButton;
    end;
  else
    PropertyType := ptText;
  end;

  if FDataLink.Field.FieldKind = fkLookup then
  begin
    ReadOnly := FDataLink.Field.ReadOnly;
    PropertyType := ptCustom;
    EditLink := (InspectorBar as TDBInspectorBar).FDBLookupIL;
  end
  else
    ReadOnly := not FDataLink.Field.CanModify;
end;

procedure TDBInspectorItem.SetPictureField(const Value: Boolean);
begin
  FPictureField := Value;
end;

procedure TDBInspectorItem.Update;
begin
  if not FDataLink.ReadOnly then
  begin
    try
      // tell data link to update database
      FDataLink.UpdateRecord;
      FDataLink.Modified;
    except
    end;
  end;
end;

{ TDBInspectorNavigator }

procedure TDBInspectorNavigator.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TDBInspectorNavigator.Create(AOwner: TComponent);
begin
  inherited Create;
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHoverButtonColor := clNone;
  FHoverButtonDownColor := clNone;

  FHints := TStringList.Create;

  FHints.Add('First record');
  FHints.Add('Prior record');
  FHints.Add( 'Next record');
  FHints.Add('Last record');
  FHints.Add('Insert record');
  FHints.Add('Delete record');
  FHints.Add('Edit record');
  FHints.Add('Post edit');
  FHints.Add('Cancel edit');
  FHints.Add('Refresh data');
  FButtonSize := 12;

end;

destructor TDBInspectorNavigator.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TDBInspectorNavigator.SetButtonSize(const Value: Integer);
begin
  FButtonSize := Value;
  Changed;
end;

procedure TDBInspectorNavigator.SetButtonStyle(const Value: TButtonStyle);
begin
  FButtonStyle := Value;
  if Value = bsDefault then
    FButtonSize := 12
  else
    FButtonSize := 15;
  Changed;
end;

procedure TDBInspectorNavigator.SetHints(const Value: TStringList);
begin
  FHints.Assign(Value);
end;

procedure TDBInspectorNavigator.SetVisible(const Value: TButtonSet);
begin
  FVisibleButtons := Value;
  Changed;
end;

{ TInspectorDataLink }

procedure TInspectorDataLink.ActiveChanged;
begin
  inherited;

  if not Active then
  begin
    FPanel.BtnBack := False;
    FPanel.BtnInsDel := False;
    FPanel.BtnFrwd := False;
    FPanel.BtnPost := False;
    FPanel.BtnEdit := False;
    FPanel.InspectorBar.Invalidate;
  end
  else
  begin
    DataSetChanged;
    EditingChanged;
  end;
end;

constructor TInspectorDataLink.Create(APanel: TDBInspectorPanel);
begin
  inherited Create;
  FPanel := APanel;
end;

procedure TInspectorDataLink.DataSetChanged;
begin
  inherited;
  FPanel.BtnBack := Active and not DataSet.BOF;
  FPanel.BtnFrwd := Active and not DataSet.EOF;
  FPanel.BtnInsDel := Active and DataSet.CanModify and
    not (DataSet.BOF and DataSet.EOF);
  FPanel.InspectorBar.Invalidate;
end;

destructor TInspectorDataLink.Destroy;
begin
  inherited;
end;

procedure TInspectorDataLink.EditingChanged;
var
  CanModify: Boolean;
begin
  inherited;
  if Assigned(DataSet) then
  begin
    CanModify := Active and DataSet.CanModify;
    FPanel.BtnInsDel := CanModify;
    FPanel.BtnEdit := CanModify and not Editing;
    FPanel.BtnPost := CanModify and Editing;
    FPanel.InspectorBar.Invalidate;
  end;
end;


{ TDBLookupInspectorEditLink }

constructor TDBLookupInspectorEditLink.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := nil;
end;

procedure TDBLookupInspectorEditLink.CreateEditor(AParent: TWinControl);
begin
  inherited;

  FEdit := TDBLookupComboBox.Create(AParent);
  FEdit.Parent := AParent;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.OnCloseUp := LookupChanged;
end;

destructor TDBLookupInspectorEditLink.Destroy;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := nil;

  inherited;
end;

procedure TDBLookupInspectorEditLink.DestroyEditor;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := nil;

  inherited;
end;

function TDBLookupInspectorEditLink.GetEditor: TWinControl;
begin
  Result := FEdit;
end;

procedure TDBLookupInspectorEditLink.LookupChanged(Sender: TObject);
begin
  FItem.EditChange;
end;

procedure TDBLookupInspectorEditLink.SetOriginalValue;
begin
  inherited;
end;

procedure TDBLookupInspectorEditLink.SetProperties(R: TRect;
  Item: TInspectorItem);
begin
  FEdit.DataSource := ((Item as TDBInspectorItem).InspectorPanel as TDBInspectorPanel).DataSource;
  FEdit.DataField := (Item as TDBInspectorItem).DataField;

  inherited;

  InflateRect(R,-2,-1);
  R.Right := R.Right - 20;

  FEdit.Left := R.Left;
  FEdit.Top := R.Top;
  FEdit.Width := R.Right - R.Left;
  FEdit.Height := R.Bottom - R.Top;
  FEdit.Font.Assign(Item.InspectorPanel.Font);
end;

procedure TDBLookupInspectorEditLink.StartEdit(Item: TInspectorItem);
begin
  inherited;
  FItem := (Item as TDBInspectorItem);
  FEdit.Visible := True;
  FEdit.SetFocus;

end;

procedure TDBLookupInspectorEditLink.StopEdit(Item: TInspectorItem);
begin
  inherited;
  FEdit.Visible := False;
end;

end.
