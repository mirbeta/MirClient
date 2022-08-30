{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               } 
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

unit W7Images;

interface

{$I TMSDEFS.INC}

uses
  Windows,  Classes, Controls, Graphics, W7Classes, W7Common, W7Graphics, Forms;

type
  TW7ImageViewMode = (ivmFitToSize, ivmNormal, ivmStretch, ivmCustom);

  TW7CustomImage = class(TW7GraphicControl)
  private
    FHorizonatlAlignment: TW7HorizontalAlignment;
    FVerticalAlignment: TW7VerticalAlignment;
    FPicture: TPicture;
    FOpacity: byte;
    FViewMode: TW7ImageViewMode;
    FDstRect: TRect;
    FCustomViewRect: TRect;
    FZoom: integer;
    FLocked: boolean;
    FTransparent: boolean;
    procedure SetHorizonatalAlignment(Value: TW7HorizontalAlignment);
    procedure SetVerticalAlignment(Value: TW7VerticalAlignment);
    procedure SetPicture(Value: TPicture);
    procedure SetOpacity(Value: byte);
    procedure SetViewMode(Value: TW7ImageViewMode);
    procedure SetCustomViewRect(Value: TRect);
    procedure SetZoom(Value: integer);
    procedure SetTransparent(Value: boolean);
  protected
    NeedPictureAssign: boolean;
    procedure GetRect;
    procedure Paint; override;
    procedure PictureChanged(Sender: TObject);
  public
    InternalBitmap: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearPictureCache;
    property HorizontalAlignment: TW7HorizontalAlignment read FHorizonatlAlignment write SetHorizonatalAlignment default whaLeft;
    property VerticalAlignment: TW7VerticalAlignment read FVerticalAlignment write SetVerticalAlignment default wvaTop;
    property Picture: TPicture read FPicture write SetPicture;
    property Opacity: byte read FOpacity write SetOpacity default 255;
    property ViewMode: TW7ImageViewMode read FViewMode write SetViewMode default ivmFitToSize;
    property CustomViewRect: TRect read FCustomViewRect write SetCustomViewRect;
    property Zoom: integer read FZoom write SetZoom default 100;
    property Locked: boolean read FLocked write FLocked default False;
    property Transparent: boolean read FTransparent write SetTransparent default False;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7Image = class (TW7CustomImage)
  published
    property HorizontalAlignment;
    property VerticalAlignment;
    property Picture;
    property Opacity;
    property ViewMode;
    property CustomViewRect;
    property Zoom;
    property Transparent;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TW7CustomImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FHorizonatlAlignment := whaLeft;
  FVerticalAlignment := wvaTop;
  FOpacity := 255;
  FViewMode := ivmFitToSize;
  InternalBitmap := TBitmap.Create;
  NeedPictureAssign := False;
  FZoom := 100;
  FLocked := False;
  FTransparent := False;
end;

destructor TW7CustomImage.Destroy;
begin
  FPicture.Free;
  InternalBitmap.Free;
  inherited;
end;

procedure TW7CustomImage.GetRect;
var
  WidthDelta: integer;
  HeightDelta: integer;
  NewWidth: integer;
  NewHeight: integer;
begin
  if (InternalBitmap.Width = 0) or (InternalBitmap.Height = 0) then
    Exit;
  case ViewMode of
    ivmFitToSize:
    begin
      NewWidth := Width;
      NewHeight := Height;
      if (InternalBitmap.Width < Width) and (InternalBitmap.Height < Height) then
      begin
        WidthDelta := Round((Width - InternalBitmap.Width) / (InternalBitmap.Width / 100));
        HeightDelta := Round((Height - InternalBitmap.Height) / (InternalBitmap.Height / 100));
        if WidthDelta < HeightDelta then
        begin
          NewWidth := Width;
          NewHeight := Round(InternalBitmap.Height + (InternalBitmap.Height / 100) * WidthDelta);
        end
        else
        begin
          NewHeight := Height;
          NewWidth := Round(InternalBitmap.Width + ((InternalBitmap.Width / 100) * HeightDelta));
        end;
      end
      else if (InternalBitmap.Width > Width) and (InternalBitmap.Height < Height) then
      begin
        WidthDelta := InternalBitmap.Width - Width;
        NewWidth := Width;
        NewHeight := Round(InternalBitmap.Height - ((InternalBitmap.Height / 100) * (WidthDelta / (InternalBitmap.Width / 100))));
      end
      else if (InternalBitmap.Width < Width) and (InternalBitmap.Height > Height) then
      begin
        HeightDelta := InternalBitmap.Height - Height;
        NewHeight := Height;
        NewWidth := Round(InternalBitmap.Width - ((InternalBitmap.Width / 100) * (HeightDelta / (InternalBitmap.Height / 100))));
      end
      else if (InternalBitmap.Width > Width) and (InternalBitmap.Height > Height) then
      begin
        WidthDelta := Round((InternalBitmap.Width - Width) / (InternalBitmap.Width / 100));
        HeightDelta := Round((InternalBitmap.Height - Height) / (InternalBitmap.Height / 100));
        if WidthDelta > HeightDelta then
        begin
          NewWidth := Width;
          NewHeight := Round(InternalBitmap.Height - ((InternalBitmap.Height / 100) * WidthDelta));
        end
        else
        begin
          NewHeight := Height;
          NewWidth := Round(InternalBitmap.Width - ((InternalBitmap.Width / 100) * HeightDelta));
        end;
      end;
      FDstRect.Left := Width div 2 - NewWidth div 2;
      FDstRect.Top := Height div 2 - NewHeight div 2;
      FDstRect.Right := FDstRect.Left + NewWidth;
      FDstRect.Bottom := FDstRect.Top + NewHeight;
      FCustomViewRect := FDstRect;
    end;
    ivmNormal:
    begin
      case FHorizonatlAlignment of
        whaLeft:
        begin
          FDstRect.Left := 0;
          FDstRect.Right := InternalBitmap.Width;
        end;
        whaCenter:
        begin
          FDstRect.Left := Width div 2 - InternalBitmap.Width div 2;
          FDstRect.Right := FDstRect.Left + InternalBitmap.Width;
        end;
        whaRight:
        begin
          FDstRect.Right := Width;
          FDstRect.Left := Width - InternalBitmap.Width;
        end;
      end;
      case FVerticalAlignment of
        wvaTop:
        begin
          FDstRect.Top := 0;
          FDstRect.Bottom := InternalBitmap.Height;
        end;
        wvaCenter:
        begin
          FDstRect.Top := Height div 2 - InternalBitmap.Height div 2;
          FDstRect.Bottom := FDstRect.Top + InternalBitmap.Height;
        end;
        wvaBottom:
        begin
          FDstRect.Bottom := Height;
          FDstRect.Top := FDstRect.Bottom - InternalBitmap.Height;
        end;
      end;
      FCustomViewRect := FDstRect;
    end;
    ivmStretch:
    begin
      FDstRect := Rect(0, 0, Width, Height);
      FCustomViewRect := FDstRect;
    end;
    ivmCustom:
    begin
      FDstRect := FCustomViewRect;
    end;
  end;
  if (FZoom <> 100) and (FViewMode <> ivmFitToSize) and (FViewMode <> ivmStretch) then
  begin
    NewWidth := Round((FDstRect.Right - FDstRect.Left) / 100 * FZoom);
    NewHeight := Round((FDstRect.Bottom - FDstRect.Top) / 100 * FZoom);
    if FViewMode = ivmCustom then
    begin
      FDstRect.Left := FDstRect.Left - abs(InternalBitmap.Width - NewWidth) div 2;
      FDstRect.Top := FDstRect.Top - abs(InternalBitmap.Height - NewHeight) div 2;
      FDstRect.Right := FDstRect.Left + NewWidth;
      FDstRect.Bottom := FDstRect.Top + NewHeight;
    end
    else
    begin
      case FHorizonatlAlignment of
        whaLeft: FDstRect.Right := FDstRect.Right + NewWidth - InternalBitmap.Width;
        whaCenter:
        begin
          FDstRect.Left := Width div 2 - NewWidth div 2;
          FDstRect.Right := FDstRect.Left + NewWidth;
        end;
        whaRight: FDstRect.Left := FDstRect.Left - NewWidth + InternalBitmap.Width;
      end;
      case FVerticalAlignment of
        wvaTop: FDstRect.Bottom := FDstRect.Bottom + NewHeight - InternalBitmap.Height;
        wvaCenter:
        begin
          FDstRect.Top := Height div 2 - NewHeight div 2;
          FDstRect.Bottom := FDstRect.Top + NewHeight;
        end;
        wvaBottom: FDstRect.Top := FDstRect.Top - NewHeight + InternalBitmap.Height;
      end;
    end;
  end;
end;

procedure TW7CustomImage.Paint;
begin
  inherited;
  if FLocked then
    Exit;

  if Assigned(FPicture.Graphic) then
    if (FPicture.Graphic.Width <> InternalBitmap.Width) or (FPicture.Graphic.Height <> InternalBitmap.Height) then
      NeedPictureAssign := True;

  if NeedPictureAssign then
  begin
    with FPicture do
    begin
      if Assigned(FPicture.Graphic) then
      begin
        InternalBitmap.PixelFormat := pf32Bit;
        InternalBitmap.Width := Graphic.Width;
        InternalBitmap.Height := Graphic.Height;
        InternalBitmap.Canvas.Brush.Color := clBlack;
        InternalBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
        InternalBitmap.Canvas.Draw(0, 0, Graphic);
      end;


      NeedPictureAssign := False;
    end;
  end;

  if Assigned(FPicture.Graphic) then
  begin
    GetRect;

    AlphaBlendBitmap(InternalBitmap, Canvas, FDstRect, FOpacity, not FTransparent);
  end;

  if (csDesigning in ComponentState) then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TW7CustomImage.PictureChanged(Sender: TObject);
begin
  NeedPictureAssign := true;
  Repaint;
end;

procedure TW7CustomImage.SetHorizonatalAlignment(Value: TW7HorizontalAlignment);
begin
  FHorizonatlAlignment := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetVerticalAlignment(Value: TW7VerticalAlignment);
begin
  FVerticalAlignment := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  NeedPictureAssign := True;
  Invalidate;
end;

procedure TW7CustomImage.SetOpacity(Value: byte);
begin
  FOpacity := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetViewMode(Value: TW7ImageViewMode);
begin
  FViewMode := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetCustomViewRect(Value: TRect);
begin
  FCustomViewRect := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetZoom(Value: integer);
begin
  FZoom := Value;
  Invalidate;
end;

procedure TW7CustomImage.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TW7CustomImage.ClearPictureCache;
begin
  FPicture.Free;
  FPicture := TPicture.Create;
end;

end.