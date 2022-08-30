{*************************************************************************}
{ THeaderItem Class                                                        }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
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

unit GDIPHeaderItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics,
  GDIPCustomItem, GDIPBase, AdvGDIP,
  GDIPImageSectionItem, ExtCtrls, SysUtils, GDIPFill,
  AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  THeaderItem = class;

  THeaderItem = class(TImageSectionItem, ITMSTones, ITMSStyle)
  private
    FHeaderColor: TColor;
    procedure SetHeaderColor(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    function GetVersionNr: integer; override;
  public
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function FirstTab(Backwards: Boolean): Boolean; override;
  published
    { Published declarations }
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default $C9C9C9;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(THeaderItem);
end;

{ THeaderItem }

procedure THeaderItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is THeaderItem then
  begin
    FHeaderColor := (Source as THeaderItem).HeaderColor;
    Changed;
  end;
end;

constructor THeaderItem.Create(AOwner: TComponent);
begin
  inherited;
  FHeaderColor := $C9C9C9;
end;

function THeaderItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := THeaderItem.Create(AOwner);
end;

class function THeaderItem.CustomClassName: String;
begin
  Result := 'Header Item';
end;

destructor THeaderItem.Destroy;
begin
  inherited;
end;

procedure THeaderItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF);
var
  b: TGPSolidBrush;
  ir: TGPRectF;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    if HeaderColor <> clNone then
    begin
      b := TGPSolidBrush.Create(MakeColor(255, HeaderColor));
      g.FillRectangle(b, r);
      b.Free;
    end;

    DrawLine(g, r, ItemAppearance);
    ir := DrawText(g, r, ItemAppearance);
    case Status.Position of
      spItemRectangle:  DrawStatus(g, r, ItemAppearance);
      spItemText: DrawStatus(g, ir, ItemAppearance);
    end;

    DrawImage(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

function THeaderItem.FirstTab(Backwards: Boolean): Boolean;
begin
  ProcessTab(Backwards);
  Result := True;
end;

function THeaderItem.GetClassType: TComponentClass;
begin
  Result := THeaderItem;
end;

function THeaderItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THeaderItem.SetColorTones(ATones: TColorTones);
begin
  CaptionColor := ATones.Selected.TextColor;
  HeaderColor := ATones.Selected.BrushColor;
end;

procedure THeaderItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  CaptionColor := clNone;
  HeaderColor := $C9C9C9;
end;

procedure THeaderItem.SetHeaderColor(const Value: TColor);
begin
  if FHeaderColor <> Value then
  begin
    FHeaderColor := Value;
    Changed;
  end;
end;

end.
