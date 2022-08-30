{***************************************************************************}
{ TTaskDialogEx component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2011                                        }
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

unit TaskDialogEx;

{$I TMSDEFS.INC}                  

interface

uses
  Classes, Windows, Messages, Forms, Dialogs, SysUtils, StdCtrls, Graphics, Consts, Math,
  ExtCtrls, Controls, TaskDialog, AdvGlowButton, AdvOfficeButtons, AdvStyleIF;

type
  TButtonCreatedEvent = procedure(Sender: TObject; Button: TAdvGlowButton) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTaskDialogEx = class(TAdvTaskDialog, ITMSStyle)
  private
    FOnButtonCreated: TButtonCreatedEvent;
    FAppearance: TGlowButtonAppearance;
    FStyle: TTMSStyle;
  protected
    function CreateRadioButton(AOwner: TComponent): TWinControl; override;
    procedure SetRadioButtonState(Btn: TWinControl; Checked: boolean); override;
    procedure SetRadioButtonCaption(Btn: TWinControl; Value: string); override;
    function CreateButton(AOwner: TComponent): TWinControl; override;
    procedure InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent); override;
    procedure SetButtonCaption(aButton: TWinControl; Value: TCaption); override;
    procedure SetButtonCancel(aButton: TWinControl; Value: Boolean); override;
    procedure SetButtonDefault(aButton: TWinControl; Value: Boolean); override;
    procedure SetButtonModalResult(aButton: TWinControl; Value: Integer); override;
    function GetButtonModalResult(aButton: TWinControl): Integer; override;
  public
    procedure SetComponentStyle(AStyle: TTMSStyle);
    property Appearance: TGlowButtonAppearance read FAppearance write FAppearance;
    property Style: TTMSStyle read FStyle write FStyle default tsCustom;
    property OnButtonCreated:TButtonCreatedEvent read FOnButtonCreated write FOnButtonCreated;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvInputTaskDialogEx = class(TAdvInputTaskDialog, ITMSStyle)
  private
    FOnButtonCreated: TButtonCreatedEvent;
    FAppearance: TGlowButtonAppearance;
    FStyle: TTMSStyle;
  protected
    function CreateRadioButton(AOwner: TComponent): TWinControl; override;
    procedure SetRadioButtonState(Btn: TWinControl; Checked: boolean); override;
    procedure SetRadioButtonCaption(Btn: TWinControl; Value: string); override;
    function CreateButton(AOwner: TComponent): TWinControl; override;
    procedure InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent); override;
    procedure SetButtonCaption(aButton: TWinControl; Value: TCaption); override;
    procedure SetButtonCancel(aButton: TWinControl; Value: Boolean); override;
    procedure SetButtonDefault(aButton: TWinControl; Value: Boolean); override;
    procedure SetButtonModalResult(aButton: TWinControl; Value: Integer); override;
    function GetButtonModalResult(aButton: TWinControl): Integer; override;
  public
    procedure SetComponentStyle(AStyle: TTMSStyle);
    property Appearance: TGlowButtonAppearance read FAppearance write FAppearance;
    property Style: TTMSStyle read FStyle write FStyle default tsCustom;
    property OnButtonCreated:TButtonCreatedEvent read FOnButtonCreated write FOnButtonCreated;
  end;


procedure Register;

implementation

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('TMS',[TAdvTaskDialogEx, TAdvInputTaskDialogEx]);
end;

//------------------------------------------------------------------------------

{ TAdvTaskDialogEx }

function TAdvTaskDialogEx.CreateButton(AOwner: TComponent): TWinControl;
begin
  Result := TAdvGlowButton.Create(AOwner);
  if Assigned(FAppearance) then
    (Result as TAdvGlowButton).Appearance := FAppearance;

  if FStyle <> tsCustom then
    (Result as TAdvGlowButton).SetComponentStyle(FStyle);

  (Result as TAdvGlowButton).TabStop := true;
  if Assigned(FOnButtonCreated) then
    FOnButtonCreated(Self,(Result as TAdvGlowButton));
end;

//------------------------------------------------------------------------------

function TAdvTaskDialogEx.CreateRadioButton(AOwner: TComponent): TWinControl;
begin
  Result := TAdvOfficeRadioButton.Create(AOwner);
end;

//------------------------------------------------------------------------------

function TAdvTaskDialogEx.GetButtonModalResult(
  aButton: TWinControl): Integer;
begin
  Result := mrNone;
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  Result := TAdvGlowButton(aButton).ModalResult;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetButtonCancel(aButton: TWinControl;
  Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Cancel := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetComponentStyle(AStyle: TTMSStyle);
begin
  FStyle := AStyle;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetButtonCaption(aButton: TWinControl;
  Value: TCaption);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Caption := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetButtonDefault(aButton: TWinControl;
  Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Default := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetButtonModalResult(aButton: TWinControl;
  Value: Integer);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).ModalResult := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetRadioButtonCaption(Btn: TWinControl;
  Value: string);
begin
  TAdvOfficeRadioButton(Btn).Caption := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.SetRadioButtonState(Btn: TWinControl;
  Checked: boolean);
begin
  TAdvOfficeRadioButton(Btn).Checked := Checked;
end;

//------------------------------------------------------------------------------

procedure TAdvTaskDialogEx.InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent);
begin
  with TRadioButton(Btn) do
  begin
    Name := 'Radio' + inttostr(btnIndex);
    Parent := AOwner;
    Font.Name := AOwner.Canvas.Font.Name;
    Font.Size := 8;
    BiDiMode := AOwner.BiDiMode;
    OnClick := OnClickEvent;

    {
    BoundsRect := TextRect;
    Left := FHorzParaMargin + FHorzMargin; //ALeft + FHorzMargin;
    Top := Y;
    Width := Self.Width - Left - 4;
    GetTextSize(Canvas, Caption, k, l);
    w := Max(w, Left + k + FHorzMargin + 20);
    }
  end;
end;

{ TAdvInputTaskDialogEx }

//------------------------------------------------------------------------------

function TAdvInputTaskDialogEx.CreateButton(AOwner: TComponent): TWinControl;
begin
  Result := TAdvGlowButton.Create(AOwner);
  if Assigned(FAppearance) then
    (Result as TAdvGlowButton).Appearance := FAppearance;

  if FStyle <> tsCustom then
    (Result as TAdvGlowButton).SetComponentStyle(FStyle);

  (Result as TAdvGlowButton).TabStop := true;
  if Assigned(FOnButtonCreated) then
    FOnButtonCreated(Self,(Result as TAdvGlowButton));
end;

function TAdvInputTaskDialogEx.CreateRadioButton(
  AOwner: TComponent): TWinControl;
begin
  Result := TAdvOfficeRadioButton.Create(AOwner);
end;

function TAdvInputTaskDialogEx.GetButtonModalResult(
  aButton: TWinControl): Integer;
begin
  Result := mrNone;
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  Result := TAdvGlowButton(aButton).ModalResult;
end;

procedure TAdvInputTaskDialogEx.SetButtonCancel(aButton: TWinControl;
  Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Cancel := Value;
end;

procedure TAdvInputTaskDialogEx.SetButtonCaption(aButton: TWinControl;
  Value: TCaption);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Caption := Value;
end;

procedure TAdvInputTaskDialogEx.SetButtonDefault(aButton: TWinControl;
  Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Default := Value;
end;

procedure TAdvInputTaskDialogEx.SetButtonModalResult(aButton: TWinControl;
  Value: Integer);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).ModalResult := Value;
end;


procedure TAdvInputTaskDialogEx.SetComponentStyle(AStyle: TTMSStyle);
begin
  FStyle := AStyle;
end;

procedure TAdvInputTaskDialogEx.SetRadioButtonCaption(Btn: TWinControl;
  Value: string);
begin
  TAdvOfficeRadioButton(Btn).Caption := Value;
end;

procedure TAdvInputTaskDialogEx.SetRadioButtonState(Btn: TWinControl;
  Checked: boolean);
begin
  TAdvOfficeRadioButton(Btn).Checked := Checked;
end;

procedure TAdvInputTaskDialogEx.InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent);
begin
  with TRadioButton(Btn) do
  begin
    Name := 'Radio' + inttostr(btnIndex);
    Parent := AOwner;
    Font.Name := AOwner.Canvas.Font.Name;
    Font.Size := 8;
    BiDiMode := AOwner.BiDiMode;
    OnClick := OnClickEvent;
    {
    BoundsRect := TextRect;
    Left := FHorzParaMargin + FHorzMargin; //ALeft + FHorzMargin;
    Top := Y;
    Width := Self.Width - Left - 4;
    GetTextSize(Canvas, Caption, k, l);
    w := Max(w, Left + k + FHorzMargin + 20);
    }
  end;
end;

end.
