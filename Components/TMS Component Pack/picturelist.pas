{***************************************************************************}
{ TPictureList component                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2014                                        }
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

unit PictureList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls;

type
  TThumbnails = class;

  TThumbnailSource = (tsPicture,tsFile);

  TThumbnail = class(TCollectionItem)
  private
    FPicture: TPicture;
    FFilename: string;
    FSource: TThumbnailSource;
    FCaption: string;
    FTag: Integer;
    procedure SetPicture(const Value: TPicture);
    procedure SetFileName(const Value: string);
    procedure SetCaption(const Value: string);
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  protected
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
	procedure Assign(Source: TPersistent); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Source: TThumbnailSource read FSource write FSource;
    property Filename: string read FFilename write SetFileName;
    property Caption: string read FCaption write SetCaption;
    property Tag: Integer read FTag write FTag;
  end;

  TPictureList = class;

  TThumbnails = class(TCollection)
  private
    FOwner: TPictureList;
    FThumbnailCount: Integer;
    function GetItem(Index: Integer): TThumbnail;
    procedure SetItem(Index: Integer; const Value: TThumbnail);
    procedure Changed(Sender:TObject);
  protected
    function GetOwner: TPersistent; override;
    procedure AddThumb;
    procedure RemoveThumb;
  public
    constructor Create(AOwner:TPictureList);
    function Add:TThumbnail;
    function Insert(index:integer): TThumbnail;
    property Items[Index: Integer]: TThumbnail read GetItem write SetItem; default;
  end;

  TThumbnailOrientation = (toVertical, toHorizontal);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPictureList = class(TCustomListBox)
  private
    FThumbnails: TThumbnails;
    FUpdateCount: Integer;
    FShowCaption: Boolean;
    FOrientation: TThumbnailOrientation;
    FThumbnailSize: Integer;
    FBuffered: Boolean;
    FShowSelection: Boolean;
    FInSizing: Boolean;
    { Private declarations }
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetThumbnails(const Value: TThumbnails);
    procedure BuildItems;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetOrientation(const Value: TThumbnailOrientation);
    procedure SetThumbnailSize(const Value: Integer);
    procedure SetShowSelection(const Value: Boolean);
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; ARect: TRect;AState: TOwnerDrawState); override;
    procedure CreateParams(var Params:TCreateParams); override;
    procedure UpdateHorzScroll;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ShowFolder(FolderName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property BorderStyle;
    property Buffered: Boolean read FBuffered write FBuffered default True;
    property Color;
    property Columns;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MultiSelect;
    property Orientation: TThumbnailOrientation read FOrientation
      write SetOrientation default toVertical;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption: Boolean read FShowCaption
      write SetShowCaption default True;
    property ShowHint;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection;
    property TabOrder;
    property TabStop;
    property Thumbnails: TThumbnails read FThumbnails write SetThumbnails;
    property ThumbnailSize: Integer read FThumbnailSize write SetThumbnailSize;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;



implementation


procedure TPictureList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;


constructor TPictureList.Create(AOwner: TComponent);
begin
  inherited;
  FThumbnails := TThumbnails.Create(Self);
  Style := lbOwnerDrawFixed;
  FThumbnailSize := 64;
  FBuffered := True;
  FShowCaption := True;
end;

destructor TPictureList.Destroy;
begin
  FThumbnails.Free;
  inherited;
end;

procedure TPictureList.DrawItem(Index: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  Rx, Ry: Double;
  Width,Height, TH: Integer;
  DrawRect: TRect;
  TempPic, Pic: TPicture;
begin
  if (odSelected in AState) and FShowSelection then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
    Canvas.Pen.Width := 2;
    Canvas.Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    Canvas.Pen.Width := 2;
    Canvas.Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
  end;

  //Make sure the focus rect. is redrawn
  Canvas.MoveTo(ARect.Left,ARect.Top);
  Canvas.LineTo(ARect.Right,ARect.Top);
  Canvas.LineTo(ARect.Right,ARect.Bottom);
  Canvas.LineTo(ARect.Left,ARect.Bottom);
  Canvas.LineTo(ARect.Left,ARect.Top);

  TH := 0;

  InflateRect(ARect,-2,-2);

  if (Index >= 0) and (Index < FThumbnails.Count) then
  begin
    with FThumbnails.Items[Index] do
    begin
      TempPic := nil;
      Pic := Picture;

      if FBuffered then
      begin
        if not Assigned(Picture.Graphic) and (Filename <> '') and (Source = tsFile) then
        begin
          Picture.LoadFromFile(Filename);
          Pic := Picture;
        end;
      end;

      if FShowCaption then
      begin
        DrawTextEx(Canvas.Handle,pchar(Caption),Length(Caption),ARect,DT_CENTER or DT_BOTTOM or DT_SINGLELINE or DT_END_ELLIPSIS,nil);
        TH := Canvas.TextHeight('gh');
        ARect.Bottom := ARect.Bottom - TH;
      end;

      Width := ARect.Right - ARect.Left;
      Height := ARect.Bottom - ARect.Top;

      if (Width > 0) and (Height > 0) then
      begin
        rx := Pic.Width/Width;
        ry := Pic.Height/Height;

        if (rx > 1) or (ry > 1) then
        begin
          if rx > ry then
            DrawRect := Rect(0,0,Width,Trunc(Pic.Height/rx))
          else
            DrawRect := Rect(0,0,Trunc(Pic.Width/ry),Height);
        end
        else
          DrawRect := Rect(0,0,Pic.Width,Pic.Height);

        {Center the drawing rectangle}
        OffsetRect(DrawRect,ARect.Left + (Width - DrawRect.Right + DrawRect.Left) shr 1,
                            ARect.Top + (Height - DrawRect.Bottom + DrawRect.Top) shr 1);

        Canvas.StretchDraw(DrawRect,Pic.Graphic);
      end;

      if Assigned(TempPic) then
        TempPic.Free;

    end;
  end;

  InflateRect(ARect,2,2);
  ARect.Bottom := ARect.Bottom + TH;
  if odFocused in AState then DrawFocusRect(Canvas.Handle,ARect);
end;

procedure TPictureList.SetOrientation(const Value: TThumbnailOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    SetThumbnailSize(FThumbnailSize);
    // Invalidate;
  end;
end;


procedure TPictureList.BuildItems;
begin
  if csLoading in ComponentState then
    Exit;

  if csDestroying in ComponentState then
    Exit;

  if FUpdateCount > 0 then
    Exit;

  FUpdateCount := 1;

  while Items.Count > FThumbnails.FThumbnailCount do
    Items.Delete(Items.Count-1);

  while Items.Count < FThumbnails.FThumbnailCount do
    Items.Add('');

  UpdateHorzScroll;
  // SetThumbnailSize(FThumbnailSize);

  FUpdateCount := 0;

  // Invalidate;
end;

procedure TPictureList.SetThumbnails(const Value: TThumbnails);
begin
  FThumbnails.Assign(Value);
end;


{ TThumbnail }

procedure TThumbnail.Changed;
begin
  (Collection as TThumbnails).Changed(Self);
end;

constructor TThumbnail.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  (Collection as TThumbnails).AddThumb;
end;

destructor TThumbnail.Destroy;
begin
  (Collection as TThumbnails).RemoveThumb;
  FPicture.Free;
  inherited;
end;

procedure TThumbnail.Assign(Source: TPersistent);
var
	thumb: TThumbnail;
begin
	if Source is TThumbnail then begin
		thumb := Source as TThumbnail;

		Picture.Assign(thumb.Picture);
		Self.Source := thumb.Source;
		Filename := thumb.Filename;
		Caption := thumb.Caption;
		Tag := thumb.Tag;
		end
	  else
		inherited Assign(Source);
end;

procedure TThumbnail.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TThumbnail.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed;
end;

procedure TThumbnail.SetFileName(const Value: string);
begin
  FFilename := Value;
  Changed;
end;

procedure TThumbnail.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

{ TThumbnails }

function TThumbnails.Add: TThumbnail;
begin
  Result := TThumbnail(inherited Add);
end;

procedure TThumbnails.AddThumb;
begin
  FThumbnailCount :=  FThumbnailCount + 1;
  FOwner.BuildItems;
end;

procedure TThumbnails.Changed(Sender: TObject);
begin
  FOwner.BuildItems;
end;

constructor TThumbnails.Create(AOwner: TPictureList);
begin
  inherited Create(TThumbnail);
  FOwner := AOwner;
  FThumbnailCount := 0;
end;

function TThumbnails.GetItem(Index: Integer): TThumbnail;
begin
  Result := TThumbnail(inherited Items[Index]);
end;

function TThumbnails.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TThumbnails.Insert(Index: Integer): TThumbnail;
begin
  Result := TThumbnail(inherited Insert(Index));
end;

procedure TThumbnails.RemoveThumb;
begin
  if FThumbnailCount >0 then
    FThumbnailCount :=  FThumbnailCount - 1;
  FOwner.BuildItems;
end;

procedure TThumbnails.SetItem(Index: Integer; const Value: TThumbnail);
begin
  inherited SetItem(Index, Value);
end;

procedure TPictureList.Loaded;
begin
  inherited;
  BuildItems;
end;

procedure TPictureList.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TPictureList.UpdateHorzScroll;
begin
  if Orientation <> toVertical then
    Columns := FThumbnails.Count;
end;

procedure TPictureList.CreateParams(var Params:TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_HSCROLL;
end;


procedure TPictureList.SetThumbnailSize(const Value: Integer);
begin
  FThumbnailSize := Value;

  if Orientation = toVertical then
  begin
    ItemHeight := Value;
    if Columns > 0 then
      SendMessage(Handle,LB_SETCOLUMNWIDTH,Value,0);
  end
  else
  begin
    ItemHeight := Height - GetSystemMetrics(SM_CYHSCROLL) - 4 * GetSystemMetrics(SM_CYBORDER);
    Columns := FThumbnails.Count;
    SendMessage(Handle,LB_SETCOLUMNWIDTH,Value,0);
  end;
end;

procedure TPictureList.WMSize(var Message: TWMSize);
var
  NewItemHeight: Integer;
begin
  inherited;


  if Orientation = toHorizontal then
  begin
    if (FThumbnailSize > 0) then
    begin
      FInSizing := True;
      SendMessage(Handle,LB_SETCOLUMNWIDTH,FThumbnailSize,0);
      FInSizing := False;
    end;

    if not (csLoading in ComponentState) then
    begin
      NewItemHeight := Height - GetSystemMetrics(SM_CYHSCROLL) - 4 * GetSystemMetrics(SM_CYBORDER);
      if NewItemHeight > 0 then
        ItemHeight := NewItemHeight;
    end;
  end
  else
  begin
    Invalidate;
  end;
end;

procedure TPictureList.ShowFolder(FolderName: string);
var
  SR: TSearchRec;

  procedure AddToList(Name:string);
  var
    Ext: string;
  begin
    Ext := Uppercase(ExtractFileExt(Name));
    if (Ext = '.JPG') or (Ext = '.JPEG') or (Ext = '.GIF') or
       (Ext = '.BMP') or (Ext = '.ICO') or (Ext = '.WMF') then
      with FThumbnails.Add do
      begin
        FileName := ExtractFilePath(FolderName) + Name;
        Source := tsFile;
        Caption := Name;
        if not FBuffered then
          Picture.LoadFromFile(Filename);
      end;
  end;

begin
  BeginUpdate;

  FThumbnails.Clear;

  if FindFirst(FolderName,faAnyFile,SR) = 0 then
  begin
    AddToList(SR.Name);
    while FindNext(SR) = 0 do
    begin
      AddToList(SR.Name);
    end;
  end;

  FindClose(SR);

  EndUpdate;
end;

procedure TPictureList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  NewC: Integer;
begin
  inherited;
  if (Columns > 0) and (Orientation = toVertical) then
  begin
    NewC := AWidth div FThumbnailSize;

    if (NewC > 0) and (NewC <> Columns) then
      Columns := NewC;
  end;
end;

procedure TPictureList.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TPictureList.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TPictureList.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      BuildItems;
  end;
end;

procedure TPictureList.WMPaint(var Message: TWMPaint);
begin
  if FUPdateCount > 0 then
  begin
    Message.Result := 1;
    Exit;
  end
  else
    inherited;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
