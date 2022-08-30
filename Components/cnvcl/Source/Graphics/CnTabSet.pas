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

unit CnTabSet;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�������˫���¼���TabSetʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnTabSet.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2007.03.06
              ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Tabs;

type
  TCnTabSetCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  TCnTabSet = class(TTabSet)
  private
    FDblClickClose: Boolean;
    FOnCloseTab: TCnTabSetCloseEvent;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure DoCloseTab(Index: Integer; var CanClose: Boolean); virtual;
  published
    property DblClickClose: Boolean read FDblClickClose write FDblClickClose;
    {* �Ƿ�˫��ʱ�Զ��رյ�ǰҳ��}
    property OnCloseTab: TCnTabSetCloseEvent read FOnCloseTab write FOnCloseTab;
    {* ˫��ʱ�Զ��ر�ҳ��ǰ�������¼�}
    property OnDblClick;
    {* ˫��ʱ�����¼�}
  end;

implementation

{ TCnTabSet }

procedure TCnTabSet.DoCloseTab(Index: Integer; var CanClose: Boolean);
begin
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index, CanClose);
end;

procedure TCnTabSet.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
  Index: Integer;
  CanClose: Boolean;
begin
  inherited;
  DblClick;

  if not FDblClickClose then
    Exit;

  P := ScreenToClient(Mouse.CursorPos);
  Index := ItemAtPos(P);
  if Index >= 0 then
  begin
    CanClose := True;
    DoCloseTab(Index, CanClose);
    
    if CanClose then
      Tabs.Delete(Index);
  end;
end;

end.
