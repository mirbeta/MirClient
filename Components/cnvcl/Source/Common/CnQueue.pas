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

unit CnQueue;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������ʵ��
* ��Ԫ���ߣ�С��
* ��    ע���򵥵���������࣬��βPush����ͷPop�����������Ƕ��󣨱�ת����ָ�룩��
*           ����ʱ�ڲ��л�����ƣ��������ⲿͨ���ٽ������⡣�������ӣ�
*           ������
*           var
*             Q: TCnQueue;
*
*           ������
*             Q := TCnQueue.Create;
*            
*           ʹ�ã�
*
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject.Create;
*             Q.Push(Data); // �������β
*           end;
*            
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject(Q.Pop); // �Ӷ���ͷ��ȡ��
*             TmpObj.Free;
*           end;
*
*           �ͷţ�
*             Q.Free;
* ����ƽ̨��PWinXP + Delphi 7
* ���ݲ��ԣ�PWin2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnQueue.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2008.04.30 V1.0
*               С���ԭʼ������ֲ������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils;

type
  TCnQueue = class
  private
    FHead: TObject;
    FTail: TObject;
    FSize: Integer;
    FLock: _RTL_CRITICAL_SECTION;
    procedure FreeNode(Value: TObject);
    function GetSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(Data: Pointer);
    function Pop: Pointer;
    property Size: Integer read GetSize;
  end;

implementation

type
  TCnNode = class
  private
    FNext: TCnNode;
    FData: Pointer;
  public
    property Next: TCnNode read FNext write FNext;
    property Data: Pointer read FData write FData;
  end;

{ TCnQueue }

procedure TCnQueue.FreeNode(Value: TObject);
var
  Tmp: TCnNode;
begin
  Tmp := TCnNode(Value).Next;
  TCnNode(Value).Free;
  if Tmp = nil then
    Exit;
  FreeNode(Tmp);
end;

constructor TCnQueue.Create;
begin
  FHead := nil;
  FTail := nil;
  FSize := 0;
  InitializeCriticalSection(FLock);
end;

destructor TCnQueue.Destroy;
begin
  if FHead <> nil then
    FreeNode(FHead);
  DeleteCriticalSection(FLock);
  inherited;
end;

function TCnQueue.Pop: Pointer;
var
  Tmp: TCnNode;
begin
  EnterCriticalSection(FLock);
  try
    Result := nil;
    if FHead = nil then
      Exit;

    Result := TCnNode(FHead).Data;
    Tmp := TCnNode(FHead).Next;
    TCnNode(FHead).Free;
    FHead := Tmp;
    
    if Tmp = nil then
      FTail := nil;
    FSize := FSize - 1;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCnQueue.Push(Data: Pointer);
var
  Tmp: TCnNode;
begin
  EnterCriticalSection(FLock);
  try
    if Data = nil then Exit;
    Tmp := TCnNode.Create;
    Tmp.Data := Data;
    Tmp.Next := nil;
    
    if FTail = nil then
    begin
      FTail := Tmp;
      FHead := Tmp;
    end
    else
    begin
      TCnNode(FTail).Next := Tmp;
      FTail := Tmp
    end;
    
    FSize := FSize + 1;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TCnQueue.GetSize: Integer;
begin
  Result := FSize;
end;

end.
