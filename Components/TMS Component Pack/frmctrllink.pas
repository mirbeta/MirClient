{**************************************************************************}
{ TADVSTRINGGRID FORM CONTROL EDITLINK                                     }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000-2012                                         }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

unit frmctrllink;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms, SysUtils,
  Dialogs, AdvGrid;

type

  TSetEditorValueEvent = procedure(Sender: TObject; Grid: TAdvStringGrid; AValue: string) of object;
  TGetEditorValueEvent = procedure(Sender: TObject; Grid: TAdvStringGrid; var AValue: string) of object;
  TSetEditorProperties = procedure(Sender: TObject; Grid: TAdvStringGrid; AControl: TWinControl) of object;
  TSetEditorFocus = procedure(Sender: TObject; Grid: TAdvStringGrid; AControl: TWinControl) of object;

  TFormControlEditLink = class(TEditLink)
  private
    FControl: TWinControl;
    FOnGetEditorValue: TGetEditorValueEvent;
    FOnSetEditorValue: TSetEditorValueEvent;
    FOnSetEditorProperties: TSetEditorProperties;
    FOnSetEditorFocus: TSetEditorFocus;
  protected
    procedure EditExit(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateEditor(AParent:TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    procedure SetFocus(Value: Boolean); override;
  published
    property Control: TWinControl read FControl write FControl;
    property OnSetEditorValue: TSetEditorValueEvent read FOnSetEditorValue write FOnSetEditorValue;
    property OnSetEditorFocus: TSetEditorFocus read FOnSetEditorFocus write FOnSetEditorFocus;
    property OnGetEditorValue: TGetEditorValueEvent read FOnGetEditorValue write FOnGetEditorValue;
    property OnSetEditorProperties: TSetEditorProperties read FOnSetEditorProperties write FOnSetEditorProperties;
  end;


implementation

type
  TMyWinControl = class(TWinControl)
  published
  end;


{ TFormControlEditLink }

constructor TFormControlEditLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControl := nil;
end;

procedure TFormControlEditLink.CreateEditor(AParent: TWinControl);
begin
  if Assigned(FControl) then
    FControl.Parent := AParent;
end;

procedure TFormControlEditLink.DestroyEditor;
begin
  if Assigned(FControl) and (EditStyle = esPopup) then
  begin
    FControl.Parent := (self.Owner as TWinControl);
    FControl.Hide;
  end;
  inherited;

end;

procedure TFormControlEditLink.EditExit(Sender: TObject);
begin
  (GetParent as TAdvStringgrid).HideInplaceEdit;
  FControl.Hide;
  if (GetParent as TAdvStringgrid).CanFocus then
    (GetParent as TAdvStringgrid).SetFocus;
end;

function TFormControlEditLink.GetEditControl: TWinControl;
begin
  if not Assigned(FControl) then
    raise Exception.Create('FormControlEditLink control not assigned');

  //FControl.Parent := Grid;
  Result := FControl;
  TMyWinControl(FControl).OnExit := EditExit;
  TMyWinControl(FControl).OnKeyDown := EditKeyDown;
end;

function TFormControlEditLink.GetEditorValue: String;
begin
  if Assigned(FOnGetEditorValue) then
    FOnGetEditorValue(Self,Grid,Result);
end;

procedure TFormControlEditLink.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FControl) then
  begin
    FControl := nil;
  end;
end;

procedure TFormControlEditLink.SetEditorValue(s: String);
begin
  if Assigned(FOnSetEditorValue) then
    FOnSetEditorValue(Self,Grid,s);
end;

procedure TFormControlEditLink.SetFocus(Value: Boolean);
begin
  inherited;
  if Value then
    if Assigned(FOnSetEditorFocus) then
      FOnSetEditorFocus(Self,Grid,FControl);
end;

procedure TFormControlEditLink.SetProperties;
begin
  inherited;
  if Assigned(FOnSetEditorProperties) then
    FOnSetEditorProperties(Self,Grid,FControl);
end;

End.



