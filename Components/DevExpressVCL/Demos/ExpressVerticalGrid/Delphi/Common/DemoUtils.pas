unit DemoUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, cxLookAndFeels;

type
  TcxLookAndFeelClickController = class
  private
    FLookAndFeelController: TcxLookAndFeelController;
    procedure LookAndFeelHandler(Sender: TObject);
  public
    constructor Create(ALookAndFeelController: TcxLookAndFeelController);
  end;

  TdxWebPageType = (wpMain, wpClienCenter, wpDownloads, wpProducts, wpSupport);

function CreateLookAndFeelMenuItems(AOwner: TComponent; ALookAndFeelContoller: TcxLookAndFeelController;
  AGroupIndex: Integer = 1): TMenuItem;
procedure HandleLookAndFeelChangeCommand(ASender: TObject; ALookAndFeelController: TcxLookAndFeelController);
procedure ShowWebPage(AWebPageType: TdxWebPageType);

implementation

uses
  cxClasses, dxCore;

const
  DXMainWebPage = 'http://www.devexpress.com';
  WebPageRelativeUrlByType: array [TdxWebPageType] of string = ('', '/ClientCenter', '/downloads', '/products/vcl', '/Support/Center');

var
  FLookAndFeelClickController: TcxLookAndFeelClickController;

procedure HandleLookAndFeelChangeCommand(ASender: TObject; ALookAndFeelController: TcxLookAndFeelController);
begin
  case TComponent(ASender).Tag of
    0..3:
      begin
        ALookAndFeelController.Kind := TcxLookAndFeelKind(TComponent(ASender).Tag);
        ALookAndFeelController.NativeStyle := False;
      end;
  else
    ALookAndFeelController.NativeStyle := True;
  end;
end;

procedure ShowWebPage(AWebPageType: TdxWebPageType);
begin
  dxShellExecute(DXMainWebPage + WebPageRelativeUrlByType[AWebPageType]);
end;

function CreateLookAndFeelMenuItems(AOwner: TComponent; ALookAndFeelContoller: TcxLookAndFeelController;
  AGroupIndex: Integer = 1): TMenuItem;

  procedure AddMenuItem(AParent: TMenuItem; ACaption: string; ATag: Integer);
  var
    AItem: TMenuItem;
  begin
    AItem := TMenuItem.Create(AParent);
    AItem.Caption := ACaption;
    AItem.RadioItem := True;
    AItem.AutoCheck := True;
    AItem.GroupIndex := AGroupIndex;
    AItem.Tag := ATag;
    AParent.Add(AItem);
    AItem.Checked := ALookAndFeelContoller.NativeStyle and (ATag = 4) or
      not ALookAndFeelContoller.NativeStyle and (Ord(ALookAndFeelContoller.Kind) = ATag);
    AItem.OnClick := FLookAndFeelClickController.LookAndFeelHandler;
  end;

begin
  if FLookAndFeelClickController = nil then
    FLookAndFeelClickController := TcxLookAndFeelClickController.Create(ALookAndFeelContoller);
  Result := TMenuItem.Create(AOwner);
  Result.Caption := '&Look&&Feel';
  AddMenuItem(Result, '&Flat', 0);
  AddMenuItem(Result, '&Standard', 1);
  AddMenuItem(Result, '&UltraFlat', 2);
  AddMenuItem(Result, '&Office11', 3);
  AddMenuItem(Result, '&Native', 4);
end;

{ TcxLookAndFeelClickController }

constructor TcxLookAndFeelClickController.Create(
  ALookAndFeelController: TcxLookAndFeelController);
begin
  inherited Create;
  FLookAndFeelController := ALookAndFeelController;
end;

procedure TcxLookAndFeelClickController.LookAndFeelHandler(Sender: TObject);
begin
  HandleLookAndFeelChangeCommand(Sender, FLookAndFeelController);
end;

initialization

finalization
  FreeAndNil(FLookAndFeelClickController);

end.
