{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 2014                                           }
{            Email : info@tmssoftware.com                               }
{            Web : http://www.tmssoftware.com                           }
{***********************************************************************}

unit PlanImageDraw;

interface

uses
  Planner, Windows, Classes, Graphics, PlanDraw, PictureContainer, AdvGDIP;

type
  TImageDrawTool = class(TCustomItemDrawTool)
  private
    FPictureContainer: TPictureContainer;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure DrawItem(PlannerItem: TPlannerItem; Canvas: TCanvas; Rect: TRect;
      Selected, Print: Boolean); override;
  published
    property PictureContainer: TPictureContainer read FPictureContainer write FPictureContainer;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner',[TImageDrawTool]);
end;

{ TImageDrawTool }

procedure TImageDrawTool.DrawItem(PlannerItem: TPlannerItem; Canvas: TCanvas;
  Rect: TRect; Selected, Print: Boolean);
var
  gr: TGraphic;
begin
  inherited;

  if PlannerItem.Selected then
  begin
    Canvas.Pen.Color := PlannerItem.BorderColor;
    Canvas.Rectangle(Rect.Left, Rect.Top + 1, Rect.Right , Rect.Bottom - 1);
    Canvas.Brush.Color := PlannerItem.TrackSelectColor;
    Canvas.Pen.Color := PlannerItem.TrackSelectColor;
    Canvas.Rectangle(Rect.Left, Rect.Top + 1, Rect.Right , Rect.Top + 1 + PlannerItem.Planner.TrackWidth);
  end
  else
  begin
    if Assigned(PlannerItem.Planner.OnPlannerBkgProp) then
      PlannerItem.Planner.OnPlannerBkgProp(PlannerItem.Planner, PLannerItem.ItemBegin, PlannerItem.ItemPos, Canvas.Brush, Canvas.Pen);

//    Canvas.Brush.Color := PlannerItem.Planner.BackgroundColor[PLannerItem.ItemBegin, PLannerItem.ItemPos];
    Canvas.Rectangle(Rect.Left, Rect.Top + 1, Rect.Right , Rect.Bottom - 1);
  end;

  if (PlannerItem.DrawTag <> '') and Assigned(PictureContainer) then
  begin
    gr := PictureContainer.FindPicture(PlannerItem.DrawTag);

    if Assigned(gr) then
    begin
      Canvas.Draw(rect.Left, rect.Top + 1 + PlannerItem.Planner.TrackWidth, gr);
    end;
  end;
end;

procedure TImageDrawTool.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AComponent = FPictureContainer) and (AOperation = opRemove) then
    FPictureContainer := nil;
end;


end.
