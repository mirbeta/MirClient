{*************************************************************************}
{ THTMLSectionItem Class                                                  }
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

unit GDIPHTMLSectionItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, SysUtils, imgList, Graphics, Classes, GDIPImageSectionItem, GDIPCustomItem,
  AdvGDIP, GDIPPictureContainer, Controls, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TAnchorEvent = procedure(Sender: TObject; Anchor: String) of object;

  TAnchorRecord = record
    CaptionAnchor: String;
    DescriptionAnchor: String;
  end;

  THTMLSectionItem = class(TImageSectionItem)
  private
    FHTMLCache: TGPBitmap;
    FRefreshHTMLCache: Boolean;
    FOnCaptionAnchorClick: TAnchorEvent;
    FOnDescriptionAnchorClick: TAnchorEvent;
    FOnInternalCaptionAnchorClick: TAnchorEvent;
    FOnInternalDescriptionAnchorClick: TAnchorEvent;
  protected
    function GetVersionNr: integer; override;
  public
    procedure UseCaption(ACaption: String); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure Changed; override;
    destructor Destroy; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    function GetAnchorAtXY(pX, pY: integer; ItemAppearance: TItemAppearance): TAnchorRecord;
    property OnInternalCaptionAnchorClick: TAnchorEvent read FOnInternalCaptionAnchorClick write FOnInternalCaptionAnchorClick;
    property OnInternalDescriptionAnchorClick: TAnchorEvent read FOnInternalDescriptionAnchorClick write FOnInternalDescriptionAnchorClick;
    procedure AssignEvents(Item: TCustomItem); override;
  published
    property OnCaptionAnchorClick: TAnchorEvent read FOnCaptionAnchorClick write FOnCaptionAnchorClick;
    property OnDescriptionAnchorClick: TAnchorEvent read FOnDescriptionAnchorClick write FOnDescriptionAnchorClick;
  end;

procedure Register;

implementation

uses
  CommCtrl, ShellApi;

{$I GDIPHTMLEngine.pas}

procedure Register;
begin
  RegisterPolyItem(THTMLSectionItem);
end;


{ THTMLSectionItem }

procedure THTMLSectionItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is THTMLSectionItem then
  begin
    OnCaptionAnchorClick := (Item as THTMLSectionItem).OnCaptionAnchorClick;
    OnDescriptionAnchorClick := (Item as THTMLSectionItem).OnDescriptionAnchorClick;
  end;
end;

procedure THTMLSectionItem.Changed;
begin
  FRefreshHTMLCache := true;
  inherited;
end;

function THTMLSectionItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := THTMLSectionItem.Create(AOwner);
end;

class function THTMLSectionItem.CustomClassName: String;
begin
  Result := 'Section HTML Item';
end;

destructor THTMLSectionItem.Destroy;
begin
  inherited;
  if Assigned(FHTMLCache) then
    FHTMLCache.Free;
end;

procedure THTMLSectionItem.DoMouseMove(Sender: TObject; Shift: TShiftState; pX,
  pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
var
  anchor: TAnchorRecord;
  anchorif: IGDIPAnchor;
begin
  inherited;
  if IsItemAtXY(pX, pY) then
  begin
    anchor := GetAnchorAtXY(pX, pY, ItemAppearance);
    if Assigned(ItemOwner) then
    begin
      if ItemOwner.GetInterface(IGDIPAnchor, anchorif) then
      begin
        anchorif.Anchor(anchor.CaptionAnchor);
        if Anchor.CaptionAnchor = '' then
          anchorif.Anchor(anchor.DescriptionAnchor);
      end;
    end;
  end;
end;

procedure THTMLSectionItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
var
  anchor: TAnchorRecord;
begin
  inherited;
  anchor := GetAnchorAtXY(pX, pY, ItemAppearance);
  if anchor.CaptionAnchor <> '' then
  begin
    if Assigned(OnInternalCaptionAnchorClick) then
      OnInternalCaptionAnchorClick(Self, anchor.CaptionAnchor);

    if Assigned(OnCaptionAnchorClick) then
      OnCaptionAnchorClick(Self, anchor.CaptionAnchor);
  end;

  if anchor.DescriptionAnchor <> '' then
  begin
    if Assigned(OnInternalDescriptionAnchorClick) then
      OnInternalDescriptionAnchorClick(Self, anchor.DescriptionAnchor);

    if Assigned(OnDescriptionAnchorClick) then
      OnDescriptionAnchorClick(Self, anchor.DescriptionAnchor);
  end;
end;

function THTMLSectionItem.DrawText(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
var
  r: TGPRectF;
  textr, textoutr: TGPRectF;
  textx, texty: Double;
  caph: Double;
  f: TFont;
  htmlr: TRect;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  htmlrt: TGPRectF;
  rgn: TGPRegion;
  htmlg: TGPGraphics;
begin
  if FRefreshHTMLCache or not ItemAppearance.DrawHTMLCache then
  begin
    if Assigned(FHTMLCache) then
      FHTMLCache.Free;

    if ItemAppearance.DrawHTMLCache then    
      r := MakeRect(0, 0, Arect.Width, ARect.Height)
    else
      r := ARect;

    FHTMLCache := TGPBitmap.Create(Round(Arect.Width), Round(ARect.Height), PixelFormat32bppARGB);
    if ItemAppearance.DrawHTMLCache then    
      htmlg := TGPGraphics.Create(FHTMLCache)
    else
      htmlg := g;
      
    htmlg.SetSmoothingMode(g.GetSmoothingMode);
    if (g.GetTextRenderingHint = TextRenderingHintClearTypeGridFit) and ItemAppearance.DrawHTMLCache then
      htmlg.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit)
    else
      htmlg.SetTextRenderingHint(g.GetTextRenderingHint);

    if IsImageVisible(ItemAppearance) then
    begin
      r.X := r.X + ImageWidth + Spacing;
      r.Width := r.Width - ImageWidth - Spacing;
    end;
    Result := r;
    if (Caption = '') and (Description = '') then
      Exit;

    with ItemAppearance do
    begin
      caph := 0;
      if Caption <> '' then
      begin
        f := TFont.Create;
        f.Assign(GetFont(ItemAppearance));
        if CaptionSize <> -1 then
          f.Size := CaptionSize;

        if CaptionFontStyle <> [] then
          f.Style := CaptionFontStyle;

        if CaptionColor <> clNone then
          f.Color := CaptionColor;

        textr := MakeRect(r.X + CaptionMargin.Left, r.Y + CaptionMargin.Top, r.Width - CaptionMargin.Right - CaptionMargin.Left,
          r.Height - CaptionMargin.Bottom - CaptionMargin.Top);

        htmlr := Bounds(Round(textr.X), Round(textr.Y), Round(textr.Width), Round(textr.Height));

        HTMLDrawGDIP(htmlg, f, Caption, htmlr,ItemAppearance.ImageList, 0,0,-1,-1,0, False,True,false,false, False,False,WordWrap and (Description = ''),1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);

        textoutr := MakeRect(0, 0, XSize, YSize);

        if Description <> '' then
          textr.Height := textoutr.Height + CaptionMargin.Bottom + CaptionMargin.Top;

        textx := 0;
        texty := 0;
        if CaptionLocation <> tlCustom then
          GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, CaptionLocation)
        else
        begin
          textx := CaptionLeft;
          texty := CaptionTop;
        end;

        textx := textx + textr.X;
        texty := texty + textr.Y;

        htmlrt := MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y));
        htmlr := Bounds(Round(htmlrt.X), Round(htmlrt.Y), Round(htmlrt.Width), Round(htmlrt.Height));
        rgn := TGPRegion.Create(htmlrt);
        htmlg.SetClip(rgn);
        HTMLDrawGDIP(htmlg, f, Caption, htmlr,ItemAppearance.ImageList, 0,0,-1,-1,0, False,False,false,false, False,False,WordWrap and (Description = ''),1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);
        htmlg.ResetClip;
        rgn.Free;

        f.Free;

        caph := textr.Height;
      end;

      Result := MakeRect(textx, texty, textoutr.Width, textoutr.Height);

      if Description <> '' then
      begin
        f := TFont.Create;
        f.Assign(GetFont(ItemAppearance));
        if DescriptionSize <> -1 then
          f.Size := DescriptionSize;

        if DescriptionFontStyle <> [] then
          f.Style := DescriptionFontStyle;

        if DescriptionColor <> clNone then
          f.Color := DescriptionColor;

        if caph = 0 then
        begin
          textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + DescriptionMargin.Top, r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
            r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top);
        end
        else
        begin
          textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + CaptionMargin.Top + DescriptionMargin.Top + caph , r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
            r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top - caph - CaptionMargin.Top);
        end;

        htmlr := Bounds(Round(textr.X), Round(textr.Y), Round(textr.Width), Round(textr.Height));

        HTMLDrawGDIP(htmlg, f, Description, htmlr,ItemAppearance.ImageList, 0,0,-1,-1,0, False,True,false,false, False,False,WordWrap,1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);

        textoutr := MakeRect(0, 0, XSize, YSize);

        textx := 0;
        texty := 0;
        if DescriptionLocation <> tlCustom then
          GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, DescriptionLocation)
        else
        begin
          textx := DescriptionLeft;
          texty := DescriptionTop;
        end;

        textx := textx + textr.X;
        texty := texty + textr.Y;

        htmlrt := MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y));
        htmlr := Bounds(Round(htmlrt.X), Round(htmlrt.Y), Round(htmlrt.Width), Round(htmlrt.Height));
        rgn := TGPRegion.Create(htmlrt);
        htmlg.SetClip(rgn);
        HTMLDrawGDIP(htmlg, f, Description, htmlr,Itemappearance.ImageList, 0,0,-1,-1,0, False,False,false,false, False,False,WordWrap,1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,Itemappearance.PictureContainer,2);
        htmlg.ResetClip;
        rgn.Free;

        f.Free;
      end;
    end;
    FRefreshHTMLCache := false;
    if ItemAppearance.DrawHTMLCache then    
      htmlg.Free;
  end;

  if Assigned(FHTMLCache) and ItemAppearance.DrawHTMLCache then
  begin
    g.DrawImage(FHTMLCache, ARect);
    Result := inherited DrawText(g, ARect, ItemAppearance, False);
  end
  else
    Result := inherited DrawText(g, ARect, ItemAppearance, False);    
end;

function THTMLSectionItem.GetAnchorAtXY(pX, pY: integer;
  ItemAppearance: TItemAppearance): TAnchorRecord;
var
  r: TGPRectF;
  textr, textoutr: TGPRectF;
  textx, texty: Double;
  caph: Double;
  f: TFont;
  htmlr: TRect;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  htmlrt: TGPRectF;
  bmp: TBitmap;
  g: TGPGraphics;
begin
  Result.CaptionAnchor := '';
  Result.DescriptionAnchor := '';
  r := MakeRect(X, Y, Width, Height);
  if PtInGPRect(r, Point(pX, pY)) then
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);
    if IsImageVisible(ItemAppearance) then
    begin
      r.X := r.X + ImageWidth + Spacing;
      r.Width := r.Width - ImageWidth - Spacing;
    end;

    if (Caption = '') and (Description = '') then
      Exit;

    with ItemAppearance do
    begin
      caph := 0;
      if Caption <> '' then
      begin
        f := TFont.Create;
        f.Assign(GetFont(ItemAppearance));
        if CaptionSize <> -1 then
          f.Size := CaptionSize;

        if CaptionFontStyle <> [] then
          f.Style := CaptionFontStyle;

        if CaptionColor <> clNone then
          f.Color := CaptionColor;

        textr := MakeRect(r.X + CaptionMargin.Left, r.Y + CaptionMargin.Top, r.Width - CaptionMargin.Right - CaptionMargin.Left,
          r.Height - CaptionMargin.Bottom - CaptionMargin.Top);

        htmlr := Bounds(Round(textr.X), Round(textr.Y), Round(textr.Width), Round(textr.Height));

        HTMLDrawGDIP(g, f, Caption, htmlr,ItemAppearance.ImageList, 0,0,-1,-1,0, False,True,false,false, False,False,WordWrap and (Description = ''),1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);

        textoutr := MakeRect(0, 0, XSize, YSize);

        if Description <> '' then
          textr.Height := textoutr.Height + CaptionMargin.Bottom + CaptionMargin.Top;

        textx := 0;
        texty := 0;
        if CaptionLocation <> tlCustom then
          GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, CaptionLocation)
        else
        begin
          textx := CaptionLeft;
          texty := CaptionTop;
        end;

        textx := textx + textr.X;
        texty := texty + textr.Y;

        htmlrt := MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y));
        htmlr := Bounds(Round(htmlrt.X), Round(htmlrt.Y), Round(htmlrt.Width), Round(htmlrt.Height));
        a := '';
        HTMLDrawGDIP(g, f, Caption, htmlr,ItemAppearance.ImageList, pX,pY,-1,-1,0, true,False,false,false, False,False,WordWrap and (Description = ''),1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);
        Result.CaptionAnchor := a;

        f.Free;

        caph := textr.Height;
      end;

      if Description <> '' then
      begin
        f := TFont.Create;
        f.Assign(GetFont(ItemAppearance));
        if DescriptionSize <> -1 then
          f.Size := DescriptionSize;

        if DescriptionFontStyle <> [] then
          f.Style := DescriptionFontStyle;

        if DescriptionColor <> clNone then
          f.Color := DescriptionColor;

        if caph = 0 then
        begin
          textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + DescriptionMargin.Top, r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
            r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top);
        end
        else
        begin
          textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + CaptionMargin.Top + DescriptionMargin.Top + caph , r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
            r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top - caph - CaptionMargin.Top);
        end;

        htmlr := Bounds(Round(textr.X), Round(textr.Y), Round(textr.Width), Round(textr.Height));

        HTMLDrawGDIP(g, f, Description, htmlr,ItemAppearance.ImageList, 0,0,-1,-1,0, False,True,false,false, False,False,WordWrap,1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,ItemAppearance.PictureContainer,2);

        textoutr := MakeRect(0, 0, XSize, YSize);

        textx := 0;
        texty := 0;
        if DescriptionLocation <> tlCustom then
          GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, DescriptionLocation)
        else
        begin
          textx := DescriptionLeft;
          texty := DescriptionTop;
        end;

        textx := textx + textr.X;
        texty := texty + textr.Y;

        htmlrt := MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y));
        htmlr := Bounds(Round(htmlrt.X), Round(htmlrt.Y), Round(htmlrt.Width), Round(htmlrt.Height));
        a := '';
        HTMLDrawGDIP(g, f, Description, htmlr,Itemappearance.ImageList, pX,pY,-1,-1,0, False,False,false,false, False,False,WordWrap,1.0,clBlue,clNone,clNone,clNone,a,s,k,XSize,YSize,l,m,hr,nil,Itemappearance.PictureContainer,2);
        Result.DescriptionAnchor := a;
        f.Free;
      end;
    end;
    g.Free;
    bmp.Free;
  end;
end;

function THTMLSectionItem.GetClassType: TComponentClass;
begin
  Result := THTMLSectionItem;
end;

function THTMLSectionItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLSectionItem.UseCaption(ACaption: String);
begin
  Caption := '<b>Section</b> <font color="clred">HTML</font> <i>Item</i>';
end;

end.
