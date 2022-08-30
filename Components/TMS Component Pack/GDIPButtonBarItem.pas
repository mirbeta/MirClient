{*************************************************************************}
{ TButtonBarItem Class                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2013                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPButtonBarItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics,
  GDIPCustomItem, GDIPBase, AdvGDIP, ExtCtrls, SysUtils,
  AdvStyleIF, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release
  //v1.1.0.0 : New : Hint and Visible property for each bar button element

type
  TButtonBarItem = class;

  TButtonBarElement = class(TCollectionItem)
  private
    FOwner: TButtonBarItem;
    FCaption: string;
    FEnabled: Boolean;
    FImage: TAdvGDIPPicture;
    FImageName: String;
    FImageIndex: integer;
    FHint: String;
    FVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageIndex(const Value: integer);
    procedure SetImageName(const Value: String);
    procedure SetHint(const Value: String);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetButtonBarOwner: TButtonBarItem;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property ImageName: String read FImageName write SetImageName;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Hint: String read FHint write SetHint;
  end;

  TButtonBarElements = class(TCollection)
  private
    FOwner: TButtonBarItem;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TButtonBarElement;
    procedure SetItem(Index: Integer; const Value: TButtonBarElement);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TButtonBarItem);
    property Items[Index: Integer]: TButtonBarElement read GetItem write SetItem; default;
    function Add: TButtonBarElement;
    function Insert(Index: Integer): TButtonBarElement;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TElementClick = procedure(Sender: TObject; Index: integer) of object;

  TButtonBarItem = class(TCustomItem)
  private
    FCaption: String;
    FFocusedElement: integer;
    FHoveredElement, FDownElement: TButtonBarElement;
    FBackGroundColor: TColor;
    FElements: TButtonBarElements;
    FBorderColor: TColor;
    FOnElementClick: TElementClick;
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetElements(const Value: TButtonBarElements);
    procedure SetBorderColor(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
    function GetVersionNr: integer; override;
    procedure ElementsChanged(Sender: TObject);
    procedure DrawElements(g: TGPGraphics; r: TGPRectF; ItemAppearance: TItemAppearance);
    procedure DoElementClick(Sender: TObject; AIndex: Integer);
    procedure DoItemHint(Sender: TObject; Item: TCustomItem; var Hint: String); override;
  public
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function ElementAtXY(pX, pY: integer; ItemAppearance: TItemAppearance): TButtonBarElement;
    function ProcessTab(Backwards: Boolean): Boolean; override;
    function IsFocusable: Boolean; override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function CountSelectable: Integer;
    function MaxSelectable: Integer;
    function MinSelectable: Integer;
    procedure UseCaption(ACaption: String); override;

    procedure DoCMMouseLeave(var Message: TMessage); override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
  published
    { Published declarations }
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default $C9C9C9;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $C9C9C9;
    property Elements: TButtonBarElements read FElements write SetElements;
    property OnElementClick: TElementClick read FOnElementClick write FOnElementClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TButtonBarItem);
end;

{ TButtonBarItem }

procedure TButtonBarItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TButtonBarItem then
  begin
    FBackGroundColor := (Source as TButtonBarItem).BackGroundColor;
    FElements.Assign((Source as TButtonBarItem).Elements);
    Changed;
  end;
end;

function TButtonBarItem.CountSelectable: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Elements.Count - 1 do
  begin
    if (Elements[I].Enabled) and (Elements[I].Visible) then
      Inc(Result);
  end;
end;

constructor TButtonBarItem.Create(AOwner: TComponent);
begin
  inherited;
  FBackGroundColor := $C9C9C9;
  FBorderColor := $C9C9C9;
  FElements := TButtonBarElements.Create(Self);
  FElements.OnChange := ElementsChanged;
end;

function TButtonBarItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TButtonBarItem.Create(AOwner);
end;

class function TButtonBarItem.CustomClassName: String;
begin
  Result := 'Button Bar Item';
end;

destructor TButtonBarItem.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TButtonBarItem.DoCMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoveredElement := nil;
  FDownElement := nil;
  Changed;
end;

procedure TButtonBarItem.DoElementClick(Sender: TObject; AIndex: Integer);
begin
  if Assigned(OnElementClick) then
    OnElementClick(Sender, AIndex);
  FFocusedElement := AIndex;
  if Assigned(OnInternalFocus) then
    OnInternalFocus(Sender, Self);
end;

procedure TButtonBarItem.DoItemHint(Sender: TObject; Item: TCustomItem;
  var Hint: String);
var
  pos: TPoint;
  lst: TPersistent;
  it: TItemAppearance;
  el: TButtonBarElement;
begin
  inherited;
  lst := Self.GetOwner;
  if Assigned(lst) and (lst is TControl) then
  begin
    pos := Mouse.CursorPos;
    pos := TControl(lst).ScreenToClient(pos);
    it.PictureContainer := nil;
    it.ImageList := nil;
    el := ElementAtXY(pos.X, pos.Y, it);
    if Assigned(el) then
      Hint := el.Hint;
  end;
end;

procedure TButtonBarItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  prev: TButtonBarElement;
begin
  if ((Key = VK_SPACE) or (Key = VK_RETURN)) and (FFocusedElement >= 0) and (FFocusedElement <= Elements.Count - 1) then
  begin
    prev := FDownElement;
    FDownElement := Elements[FFocusedElement];
    if prev <> FDownElement then
    begin
      Changed;
    end;
  end
  else
    inherited;
end;

procedure TButtonBarItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_SPACE) or (Key = VK_RETURN)) and (FFocusedElement >= 0) and (FFocusedElement <= Elements.Count - 1) then
  begin
    if Assigned(FDownElement) then
    begin
      DoElementClick(Self, FDownElement.Index);
      FDownElement := nil;
      Changed;
    end;
  end
  else
    inherited;
end;

procedure TButtonBarItem.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
var
  prev: TButtonBarElement;
begin
  inherited;
  prev := FDownElement;
  FDownElement := ElementAtXY(pX, pY, ItemAppearance);
  if prev <> FDownElement then
  begin
    Changed;
  end;
end;

procedure TButtonBarItem.DoMouseMove(Sender: TObject; Shift: TShiftState; pX,
  pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
var
  prev: TButtonBarElement;
begin
  inherited;
  prev := FHoveredElement;
  FHoveredElement := ElementAtXY(pX, pY, ItemAppearance);
  if prev <> FHoveredElement then
  begin
    Application.CancelHint;
    Changed;
  end;
end;

procedure TButtonBarItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
begin
  inherited;
  if Assigned(FDownElement) then
  begin
    DoElementClick(Self, FDownElement.Index);
    FDownElement := nil;
    Changed;
  end;
end;

procedure TButtonBarItem.DrawElements(g: TGPGraphics; r: TGPRectF; ItemAppearance: TItemAppearance);
var
  i: integer;
  mgr, spc: integer;
  ox: Double;
  img: TAdvGDIPPicture;
  bmp: TBitmap;
  h: THandle;
  ca: TCanvas;
  size: Double;
  ft: TGPFont;
  textr: TGPRectF;
  sf: TGPStringFormat;
  txtx: Double;
  b: TGPSolidBrush;
  ir: TGPRectF;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(r);
  g.SetClip(rgn);
  spc := 4;
  mgr := 3;
  ox := r.X;
  for I := 0 to Elements.Count - 1 do
  begin
    with Elements[i] do
    begin
      if Visible then
      begin
        size := 0;

        if not Image.Empty then
          size := Image.Width + mgr * 2;

        if Assigned(ItemAppearance.PictureContainer) then
        begin
          if ImageName <> '' then
          begin
            img := ItemAppearance.PictureContainer.FindPicture(ImageName);
            if Assigned(img) then
              if not img.Empty then
                size := img.Width + mgr * 2;
          end;
        end;

        if Assigned(ItemAppearance.ImageList) then
        begin
          if (ImageIndex >= 0) and (ImageIndex <= ItemAppearance.ImageList.Count - 1) then
          begin
            bmp := TBitmap.Create;
            try
              ItemAppearance.ImageList.GetBitmap(ImageIndex, bmp);
              if not bmp.Empty then
                size := bmp.Width + mgr * 2;
            finally
              bmp.Free;
            end;
          end;
        end;

        if size > 0 then
          txtx := ox + size
        else
          txtx := ox + mgr;

        if Caption <> '' then
        begin
          ft := g.MakeFont(ItemAppearance.NormalFont);
          sf := TGPStringFormat.Create;
          sf.SetHotkeyPrefix(HotkeyPrefixShow);
          g.MeasureString(Caption, Length(Caption), ft, MakeRect(0, 0, 10000, 10000), sf, textr);
          sf.free;
          ft.Free;

          size := size + textr.Width + mgr * 2;
        end;

        if size = 0 then
          size := Height - (mgr * 2);

        ir := MakeRect(ox + mgr, r.Y + mgr, size, r.Height - (mgr * 2));

        if RectanglesInterSect(r, ir) then
        begin
          if Enabled then
          begin
            if FDownElement = Elements[i] then
            begin
              ItemAppearance.ButtonDown.BeginUpdate;
              ItemAppearance.ButtonDown.Focus := FFocusedElement = I;
              ItemAppearance.ButtonDown.Focus := ItemAppearance.ButtonDown.Focus and
                ItemAppearance.Focus and (ItemAppearance.FocusedItem = Self.Index);
              ItemAppearance.ButtonDown.EndUpdate;
              ItemAppearance.ButtonDown.Fill(g, ir)
            end
            else if FHoveredElement = Elements[i] then
            begin
              ItemAppearance.ButtonHover.BeginUpdate;
              ItemAppearance.ButtonHover.Focus := FFocusedElement = I;
              ItemAppearance.ButtonHover.Focus := ItemAppearance.ButtonHover.Focus and
                ItemAppearance.Focus and (ItemAppearance.FocusedItem = Self.Index);
              ItemAppearance.ButtonHover.EndUpdate;
              ItemAppearance.ButtonHover.Fill(g, ir)
            end
            else
            begin
              ItemAppearance.ButtonNormal.BeginUpdate;
              ItemAppearance.ButtonNormal.Focus := FFocusedElement = i;
              ItemAppearance.ButtonNormal.Focus := ItemAppearance.ButtonNormal.Focus and
                ItemAppearance.Focus and (ItemAppearance.FocusedItem = Self.Index);
              ItemAppearance.ButtonNormal.EndUpdate;
              ItemAppearance.ButtonNormal.Fill(g, ir)
            end;
          end
          else
            ItemAppearance.ButtonDisabled.Fill(g, ir);

          if not Image.Empty then
            Image.GDIPDraw(g, MakeRect(ox + mgr * 2, r.Y + (r.Height -  Image.Height) / 2, Image.Width, Image.Height));

          if Assigned(ItemAppearance.PictureContainer) then
          begin
            if ImageName <> '' then
            begin
              img := ItemAppearance.PictureContainer.FindPicture(ImageName);
              if Assigned(img) then
                if not img.Empty then
                  img.GDIPDraw(g, MakeRect(ox + mgr * 2, r.Y + (r.Height -  img.Height) / 2, img.Width, img.Height));
            end;
          end;

          if Assigned(ItemAppearance.ImageList) then
          begin
            if (ImageIndex >= 0) and (ImageIndex <= ItemAppearance.ImageList.Count - 1) then
            begin
              bmp := TBitmap.Create;
              try
                ItemAppearance.ImageList.GetBitmap(ImageIndex, bmp);
                if not bmp.Empty then
                begin
                  h := g.GetHDC;
                  ca := TCanvas.Create;
                  ca.Handle := h;
                  ca.Draw(Round(ox + mgr * 2), Round(r.Y + (r.Height - bmp.Height) / 2), bmp);
                  ca.Free;
                  g.ReleaseHDC(h);
                end;
              finally
                bmp.Free;
              end;
            end;
          end;

          if Caption <> '' then
          begin
            ft := g.MakeFont(ItemAppearance.NormalFont);
            sf := TGPStringFormat.Create;
            sf.SetHotkeyPrefix(HotkeyPrefixShow);
            b := TGPSolidBrush.Create(MakeColor(255, ItemAppearance.NormalFont.Color));
            g.DrawString(Caption, Length(Caption), ft, MakePoint(txtx + mgr, r.Y + (r.Height - textr.Height) / 2), b);
            b.Free;
            sf.free;
            ft.Free;
          end;

          ox := ox + size + spc;
        end;
      end;
    end;
  end;

  g.ResetClip;
  rgn.Free;
end;

procedure TButtonBarItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF);
var
  b: TGPSolidBrush;
  p: TGPPen;
  f: TFont;
  rt: TRect;
  rtf: TGPRectF;
  ft: TGPFont;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    if BackGroundColor <> clNone then
    begin
      b := TGPSolidBrush.Create(MakeColor(255, BackGroundColor));
      g.FillRectangle(b, r);
      b.Free;
    end;

    if BorderColor <> clNone then
    begin
      p := TGPPen.Create(MakeColor(255, BorderColor));
      g.DrawRectangle(p, r);
      p.Free;
    end;

    if (FCaption <> '') and (Elements.Count = 0) then
    begin
      f := TFont.Create;
      f.Name := 'Tahoma';
      ft := g.MakeFont(f);
      g.MeasureString(FCaption, Length(FCaption), ft, r, rtf);
      rt := Bounds(Round(r.X + 5), Round(r.Y + (r.Height - rtf.Height) / 2), Round(r.Width), Round(r.Height));
      g.DrawText(FCaption, Length(FCaption), rt, f, 0);
      ft.free;
      f.Free;
    end;

    DrawElements(g, r, ItemAppearance);

    case Status.Position of
      spItemRectangle:  DrawStatus(g, r, ItemAppearance);
      spItemText: DrawStatus(g, r, ItemAppearance);
    end;

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

function TButtonBarItem.ElementAtXY(pX, pY: integer; ItemAppearance: TItemAppearance): TButtonBarElement;
var
  i: integer;
  mgr, spc: integer;
  ox: Double;
  img: TAdvGDIPPicture;
  bmp: TBitmap;
  size: Double;
  ft: TGPFont;
  textr: TGPRectF;
  sf: TGPStringFormat;
  g: TGPGraphics;
  gpbmp: TGPBitmap;
  ir: TGPRectF;
begin
  Result := nil;

  gpbmp := TGPBitmap.Create(1, 1);
  g := TGPGraphics.Create(gpbmp);

  spc := 4;
  mgr := 3;
  ox := X;
  for I := 0 to Elements.Count - 1 do
  begin
    with Elements[i] do
    begin
      if Visible then
      begin
        size := 0;

        if not Image.Empty then
          size := Image.Width + mgr * 2;

        if Assigned(ItemAppearance.PictureContainer) then
        begin
          img := ItemAppearance.PictureContainer.FindPicture(ImageName);
          if Assigned(img) then
            if not img.Empty then
              size := img.Width + mgr * 2;
        end;

        if Assigned(ItemAppearance.ImageList) then
        begin
          if (ImageIndex >= 0) and (ImageIndex <= ItemAppearance.ImageList.Count - 1) then
          begin
            bmp := TBitmap.Create;
            try
              ItemAppearance.ImageList.GetBitmap(ImageIndex, bmp);
              if not bmp.Empty then
                size := bmp.Width + mgr * 2;
            finally
              bmp.Free;
            end;
          end;
        end;

        if Caption <> '' then
        begin
          ft := g.MakeFont(ItemAppearance.NormalFont);
          sf := TGPStringFormat.Create;
          sf.SetHotkeyPrefix(HotkeyPrefixShow);
          g.MeasureString(Caption, Length(Caption), ft, MakeRect(0, 0, 10000, 10000), sf, textr);
          sf.free;
          ft.Free;

          size := size + textr.Width + mgr * 2;
        end;

        if size = 0 then
          size := Height - (mgr * 2);

        ir := MakeRect(ox + mgr, Y + mgr, size, Height - (mgr * 2));

        if Enabled and Visible and PtInGPRect(ir, Point(pX, pY)) then
        begin
          Result := Elements[i];
          break;
        end;

        ox := ox + size + spc;
      end;
    end;
  end;

  g.free;
  gpbmp.Free;
end;

procedure TButtonBarItem.ElementsChanged(Sender: TObject);
begin
  Changed;
end;

function TButtonBarItem.GetClassType: TComponentClass;
begin
  Result := TButtonBarItem;
end;

function TButtonBarItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TButtonBarItem.IsFocusable: Boolean;
begin
  Result := CountSelectable > 0;
end;

function TButtonBarItem.MaxSelectable: Integer;
var
  I: Integer;
begin
  Result := Elements.Count - 1;
  for I := Elements.Count - 1 downto 0 do
  begin
    if Elements[i].Enabled and Elements[i].Visible then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TButtonBarItem.MinSelectable: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Elements.Count - 1 do
  begin
    if Elements[i].Enabled and Elements[i].Visible then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TButtonBarItem.ProcessTab(Backwards: Boolean): Boolean;
var
  idx: integer;
begin
  if CountSelectable > 0 then
  begin
    Result := false;
    idx := FFocusedElement;

    if (idx = MaxSelectable) and not Backwards then
    begin
      FFocusedElement := MinSelectable;
      Changed;
      Result := true;
      Exit;
    end
    else if (idx = MinSelectable) and Backwards then
    begin
      FFocusedElement := MaxSelectable;
      Changed;
      Result := True;
      Exit;
    end;

    if (CountSelectable > 0) then
    begin
      if Backwards then
      begin
        Dec(idx);
        if idx < 0 then
          idx := Elements.Count - 1;
      end
      else
      begin
        Inc(idx);
        if idx > Elements.Count - 1 then
          idx := 0;
      end;
      while not Elements[idx].Enabled or not Elements[idx].Visible do
      begin
        if Backwards then
        begin
          Dec(idx);
          if idx < 0 then
            idx := Elements.Count - 1;
        end
        else
        begin
          Inc(idx);
          if idx > Elements.Count - 1 then
            idx := 0;
        end;
      end;

      if (idx >= 0) and (idx <= Elements.Count - 1) then
      begin
        FFocusedElement := idx;
        Changed;
      end;
    end;
  end
  else
    Result := True;
end;

procedure TButtonBarItem.SetBackGroundColor(const Value: TColor);
begin
  if FBackGroundColor <> Value then
  begin
    FBackGroundColor := Value;
    Changed;
  end;
end;

procedure TButtonBarItem.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TButtonBarItem.SetElements(const Value: TButtonBarElements);
begin
  if FElements <> Value then
  begin
    FElements.Assign(Value);
    Changed;
  end;
end;

procedure TButtonBarItem.UseCaption(ACaption: String);
begin
  inherited;
  FCaption := ACaption;
  Changed;
end;

{ TButtonBarElement }

procedure TButtonBarElement.Assign(Source: TPersistent);
begin
  if (Source is TButtonBarElement) then
  begin
    FCaption := (Source as TButtonBarElement).Caption;
    FEnabled := (Source as TButtonBarElement).Enabled;
    FImage.Assign((Source as TButtonBarElement).Image);
    FImageIndex := (Source as TButtonBarElement).ImageIndex;
    FImageName := (Source as TButtonBarElement).ImageName;
    FVisible := (Source as TButtonBarElement).Visible;
    FHint := (Source as TButtonBarElement).Hint;
    Changed;
  end;
end;

procedure TButtonBarElement.Changed;
begin
  FOwner.Changed;
end;

constructor TButtonBarElement.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TButtonBarElements).FOwner;
  FEnabled := True;
  FVisible := True;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;
  FOwner.Changed;
end;

destructor TButtonBarElement.Destroy;
begin
  FImage.Free;
  inherited;
  FOwner.Changed;
end;

function TButtonBarElement.GetButtonBarOwner: TButtonBarItem;
begin
  Result := FOwner;
end;

procedure TButtonBarElement.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TButtonBarElement.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TButtonBarElement.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TButtonBarElement.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TButtonBarElement.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TButtonBarElement.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TButtonBarElement.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TButtonBarElement.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TButtonBarElements }

function TButtonBarElements.Add: TButtonBarElement;
begin
  Result := TButtonBarElement(inherited Add);
end;

procedure TButtonBarElements.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

constructor TButtonBarElements.Create(AOwner: TButtonBarItem);
begin
  inherited Create(TButtonBarElement);
  FOwner := AOwner;
end;

procedure TButtonBarElements.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TButtonBarElements.GetItem(Index: Integer): TButtonBarElement;
begin
  Result := TButtonBarElement(inherited Items[Index]);
end;

function TButtonBarElements.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TButtonBarElements.Insert(Index: Integer): TButtonBarElement;
begin
  Result :=  TButtonBarElement(inherited Insert(Index));
end;

procedure TButtonBarElements.SetItem(Index: Integer;
  const Value: TButtonBarElement);
begin
  inherited Items[Index] := Value;
end;

end.
