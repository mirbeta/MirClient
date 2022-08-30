{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnAppStoreBox;
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ����������������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��Windows2003 Server + Delphi2007 up2
* ���ݲ��ԣ�Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnAppStoreBox.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2009.06.22 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, Messages,
  CnButtons;

type
  TCnAppStoreStatus = (asNotInstalled, asInstalled, asCanUpdate);

  TCnAppStoreBoxItemClick = procedure(Sender: TObject; AData: Pointer) of object;

  TCnAppStoreBox = class(TPanel)
  private
    FImage: TImage;
    FRight: TPanel;
    FCenter: TPanel;
    FLblSize: TLabel;
    FImgInstall: TImage;
    FLblInstall: TLabel;
    FBtnDown: TCnButton;
    FStatus: TCnAppStoreStatus;
    FlblName: TLabel;
    FlblDesc: TLabel;
    FOnItemDblClick: TCnAppStoreBoxItemClick;
    FOnItemClick: TCnAppStoreBoxItemClick;
    FData: Pointer;
    FOnButtonClick: TCnAppStoreBoxItemClick;
{$IFNDEF BDS2006_UP}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
{$ENDIF}
    function GetAppSize: string;
    procedure SetAppSize(const Value: string);
    procedure SetStatus(const Value: TCnAppStoreStatus);
    function GetAppDesc: string;
    function GetAppName: string;
    procedure SetAppDesc(const Value: string);
    procedure SetAppName(const Value: string);
  protected
{$IFNDEF BDS2006_UP}    
    procedure DoMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure DoMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
    procedure FOnAppStoreMouseEnter(Sender: TObject);
    procedure FOnAppStoreMouseLeave(Sender: TObject);
    procedure FOnAppStoreMouseClick(Sender: TObject);
    procedure FOnAppStoreMouseDblClick(Sender: TObject);
    procedure FOnAppStoreButtonClick(Sender: TObject);
  public
    procedure CreateWnd; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Data: Pointer read FData write FData;
  published
    property Image: TImage read FImage write FImage;
    property AppSize: string read GetAppSize write SetAppSize;
    property Status:TCnAppStoreStatus read FStatus write SetStatus;
    property AppName: string read GetAppName write SetAppName;
    property AppDesc: string read GetAppDesc write SetAppDesc;
    property OnItemClick: TCnAppStoreBoxItemClick read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TCnAppStoreBoxItemClick read FOnItemDblClick write FOnItemDblClick;
    property OnButtonClick: TCnAppStoreBoxItemClick read FOnButtonClick write FOnButtonClick;
{$IFNDEF BDS2006_UP}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
  end;

implementation

{$R CnAppStoreBox.res}

{ TCnAppStoreBox }

constructor TCnAppStoreBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWhite;
  Height := 46;
  Width := 400;

  OnMouseEnter := FOnAppStoreMouseEnter;
  OnMouseLeave := FOnAppStoreMouseLeave;

  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Height := 32;
  FImage.Width := 32;
  FImage.Align := alLeft;

{$IFDEF BDS2006_UP}  
  FImage.AlignWithMargins := True;
  FImage.Margins.Top := 7;
  FImage.Margins.Bottom := 7;
  FImage.Margins.Left := 7;
  FImage.Margins.Right := 7;
  FImage.OnMouseEnter := FOnAppStoreMouseEnter;
  FImage.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}  
  FImage.OnClick := FOnAppStoreMouseClick;
  FImage.OnDblClick := FOnAppStoreMouseDblClick;

  FRight:= TPanel.Create(Self);
  FRight.Parent := Self;
  FRight.BevelOuter := bvNone;
  FRight.Width := 242;
  FRight.Align := alRight;
  FRight.ParentColor := True;
{$IFDEF BDS2006_UP}
  FRight.OnMouseEnter := FOnAppStoreMouseEnter;
  FRight.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FRight.OnClick := FOnAppStoreMouseClick;
  FRight.OnDblClick := FOnAppStoreMouseDblClick;

  FCenter:= TPanel.Create(Self);
  FCenter.Parent := Self;
  FCenter.BevelOuter := bvNone;
  FCenter.Align := alClient;
  FCenter.ParentColor := True;
{$IFDEF BDS2006_UP}
  FCenter.OnMouseEnter := FOnAppStoreMouseEnter;
  FCenter.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FCenter.OnClick := FOnAppStoreMouseClick;
  FCenter.OnDblClick := FOnAppStoreMouseDblClick;

  FLblSize:= TLabel.Create(Self);
  FLblSize.Parent := FRight;
  FLblSize.AutoSize := False;
  FLblSize.Width := 73;
  FLblSize.Align := alLeft;
  FLblSize.Alignment := taCenter;
  FLblSize.Layout := tlCenter;
  FLblSize.Transparent := True;
{$IFDEF COMPILER6_UP}
  FLblSize.OnMouseEnter := FOnAppStoreMouseEnter;
  FLblSize.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FLblSize.OnClick := FOnAppStoreMouseClick;
  FLblSize.OnDblClick := FOnAppStoreMouseDblClick;

  FImgInstall:= TImage.Create(Self);
  FImgInstall.Parent := FRight;
  FImgInstall.Transparent := True;
  FImgInstall.Center := True;
  FImgInstall.Align := alLeft;
  FImgInstall.Width := 20;

  FLblInstall := TLabel.Create(Self);
  FLblInstall.Parent := FRight;
  FLblInstall.AutoSize := False;
  FLblInstall.Width := 73;
  FLblInstall.Align := alLeft;
  FLblInstall.Layout := tlCenter;
  FLblInstall.Transparent := True;

{$IFDEF COMPILER6_UP}
  FLblInstall.OnMouseEnter := FOnAppStoreMouseEnter;
  FLblInstall.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FLblInstall.OnClick := FOnAppStoreMouseClick;
  FLblInstall.OnDblClick := FOnAppStoreMouseDblClick;

{$IFDEF BDS2006_UP}
  FImgInstall.OnMouseEnter := FOnAppStoreMouseEnter;
  FImgInstall.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FImgInstall.OnClick := FOnAppStoreMouseClick;
  FImgInstall.OnDblClick := FOnAppStoreMouseDblClick;
  
  FBtnDown:= TCnButton.Create(Self);
  FBtnDown.Parent := FRight;
  FBtnDown.Left := 163;
  FBtnDown.Top := 11;
  FBtnDown.Height := 22;
  FBtnDown.Width := 67;
  FBtnDown.Font.Color := clRed;
  FBtnDown.Font.Style := [fsBold];
  FBtnDown.ModernBtnStyle := bsModern;
  FBtnDown.Color := 15519380;
  FBtnDown.DownBold := False;
  FBtnDown.FlatBorder := False;
  FBtnDown.HotTrackBold := False;
  FBtnDown.HotTrackColor := 16744448;
  FBtnDown.LightColor := 16574164;
  FBtnDown.ParentColor := False;
  FBtnDown.ShadowColor := clNone;
  FBtnDown.TabOrder := 10;
  FBtnDown.TabStop := True;
  
  FBtnDown.OnMouseEnter := FOnAppStoreMouseEnter;
  FBtnDown.OnMouseLeave := FOnAppStoreMouseLeave;
  FBtnDown.OnClick := FOnAppStoreButtonClick;

  FlblName:= TLabel.Create(Self);
  FlblName.Parent := FCenter;
  FlblName.AutoSize := False;
  FlblName.ParentColor := True;
  FlblName.Transparent := True;
  FlblName.Width := 280;
  FlblName.Font.Color := clBlack;
  FlblName.Font.Style := [fsBold];
  FlblName.Top := 7;
  FlblName.Left := 3;
  FlblName.Height := 13;
{$IFDEF COMPILER6_UP}
  FlblName.OnMouseEnter := FOnAppStoreMouseEnter;
  FlblName.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FlblName.OnClick := FOnAppStoreMouseClick;
  FlblName.OnDblClick := FOnAppStoreMouseDblClick;

  FlblDesc:= TLabel.Create(Self);
  FlblDesc.Parent := FCenter;
  FlblDesc.AutoSize := False;
  FlblDesc.ParentColor := True;
  FlblDesc.Transparent := True;
  FlblDesc.Width := 280;
  FlblDesc.Font.Color := clGray;
  FlblDesc.Top := 26;
  FlblDesc.Left := 3;
  FlblDesc.Height := 13;
{$IFDEF COMPILER6_UP}
  FlblDesc.OnMouseEnter := FOnAppStoreMouseEnter;
  FlblDesc.OnMouseLeave := FOnAppStoreMouseLeave;
{$ENDIF}
  FlblDesc.OnClick := FOnAppStoreMouseClick;
  FlblDesc.OnDblClick := FOnAppStoreMouseDblClick;
end;

procedure TCnAppStoreBox.CreateWnd;
begin
  inherited CreateWnd;
  Caption := EmptyStr;
end;

destructor TCnAppStoreBox.Destroy;
begin

  inherited;
end;

procedure TCnAppStoreBox.FOnAppStoreButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Data);
end;

procedure TCnAppStoreBox.FOnAppStoreMouseClick(Sender: TObject);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Data);
end;

procedure TCnAppStoreBox.FOnAppStoreMouseDblClick(Sender: TObject);
begin
  if Assigned(FOnItemDblClick) then
    FOnItemDblClick(Self, Data);
end;

{$IFNDEF BDS2006_UP}    

procedure TCnAppStoreBox.DoMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCnAppStoreBox.DoMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$ENDIF}

procedure TCnAppStoreBox.FOnAppStoreMouseEnter(Sender: TObject);
begin
  Color := $00E9E0DA;
end;

procedure TCnAppStoreBox.FOnAppStoreMouseLeave(Sender: TObject);
begin
  Color := clWhite;
end;

function TCnAppStoreBox.GetAppDesc: string;
begin
  Result := FlblDesc.Caption;
end;

function TCnAppStoreBox.GetAppName: string;
begin
  Result := FlblName.Caption;
end;

function TCnAppStoreBox.GetAppSize: string;
begin
  Result := FLblSize.Caption;
end;

procedure TCnAppStoreBox.SetAppDesc(const Value: string);
begin
  FlblDesc.Caption := Value;
end;

procedure TCnAppStoreBox.SetAppName(const Value: string);
begin
  FlblName.Caption := Value;
end;

procedure TCnAppStoreBox.SetAppSize(const Value: string);
begin
  FLblSize.Caption := Value;
end;

procedure TCnAppStoreBox.SetStatus(const Value: TCnAppStoreStatus);
begin
  FStatus := Value;
  case FStatus of
    asNotInstalled:
      begin
        FBtnDown.Caption := 'Download';
        FBtnDown.Enabled := True;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_NOT_INSTALLED');
        FLblInstall.Caption := 'Not Installed';
      end;
    asInstalled:
      begin
        FBtnDown.Caption := 'Download';
        FBtnDown.Enabled := False;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_INSTALLED');
        FLblInstall.Caption := 'Installed';
      end;
    asCanUpdate:
      begin
        FBtnDown.Caption := 'Upgrade';
        FBtnDown.Enabled := True;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_UPDATE');
        FLblInstall.Caption := 'Need Upgrade';
      end;
  end;
end;

end.
