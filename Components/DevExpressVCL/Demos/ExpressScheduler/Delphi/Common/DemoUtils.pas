unit DemoUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, RTLConsts, cxLookAndFeels, dxCoreClasses;

type

  TcxDialogsStylesClickController = class
  private
    procedure DialogsStyleHandler(Sender: TObject);
  end;

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
function CreateDialogsStylesMenuItems(AOwner: TComponent; AGroupIndex: Integer = 1): TMenuItem;
procedure HandleLookAndFeelChangeCommand(ASender: TObject; ALookAndFeelController: TcxLookAndFeelController);
procedure ShowWebPage(AWebPageType: TdxWebPageType);

implementation

uses
  cxClasses, cxSchedulerEditorFormManager, dxCore;

const
  DXMainWebPage = 'http://www.devexpress.com';
  WebPageRelativeUrlByType: array [TdxWebPageType] of string = ('', '/ClientCenter', '/downloads', '/products/vcl', '/Support/Center');

var
  FDialogsStylesClickContoller: TcxDialogsStylesClickController;
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

function AddMenuItem(AParent: TMenuItem; ACaption: string; AHandler: TNotifyEvent; ATag: Integer;
  AGroupIndex: Integer): TMenuItem;
begin
  Result := TMenuItem.Create(AParent);
  Result.Caption := ACaption;
  Result.RadioItem := True;
  Result.AutoCheck := True;
  Result.GroupIndex := AGroupIndex;
  Result.Tag := ATag;
  AParent.Add(Result);
  Result.Checked := ATag = 0;
  Result.OnClick := AHandler;
end;

function CreateLookAndFeelMenuItems(AOwner: TComponent; ALookAndFeelContoller: TcxLookAndFeelController;
  AGroupIndex: Integer = 1): TMenuItem;
var
  AHandler: TNotifyEvent;
begin
  if FLookAndFeelClickController = nil then
    FLookAndFeelClickController := TcxLookAndFeelClickController.Create(ALookAndFeelContoller);
  AHandler := FLookAndFeelClickController.LookAndFeelHandler;
  Result := TMenuItem.Create(AOwner);
  Result.Caption := '&Look&&Feel';
  AddMenuItem(Result, '&Flat', AHandler, 0, AGroupIndex);
  AddMenuItem(Result, '&Standard', AHandler, 1, AGroupIndex);
  AddMenuItem(Result, '&UltraFlat', AHandler, 2, AGroupIndex);
  AddMenuItem(Result, '&Office11', AHandler, 3, AGroupIndex);
  AddMenuItem(Result, '&Native', AHandler, 4, AGroupIndex);
end;

function CreateDialogsStylesMenuItems(AOwner: TComponent; AGroupIndex: Integer = 1): TMenuItem;
var
  I: Integer;
  AItem: TMenuItem;
  AHandler: TNotifyEvent;
begin
  if FDialogsStylesClickContoller = nil then
    FDialogsStylesClickContoller := TcxDialogsStylesClickController.Create;
  AHandler := FDialogsStylesClickContoller.DialogsStyleHandler;
  Result := TMenuItem.Create(AOwner);
  Result.Caption := 'Event Dialog Style';
  for I := 0 to cxSchedulerEditorManager.Count - 1 do
  begin
    AItem := AddMenuItem(Result,
      cxSchedulerEditorManager.Items[I].GetName, AHandler, I, AGroupIndex);
    AItem.Name := AItem.Caption;
  end;
end;

{ TcxDialogsStylesClickController }

procedure TcxDialogsStylesClickController.DialogsStyleHandler(Sender: TObject);
begin
  cxSchedulerEditorManager.CurrentEditorFormStyle := TMenuItem(Sender).Name;
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
  FreeAndNil(FDialogsStylesClickContoller);
  FreeAndNil(FLookAndFeelClickController);

end.
