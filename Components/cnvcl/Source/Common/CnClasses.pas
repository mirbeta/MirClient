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

unit CnClasses;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������ඨ�嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ������������Ļ������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnClasses.pas 480 2010-04-09 06:45:41Z zhoujingyu $
* �޸ļ�¼��2003.03.02 V1.3
*               ���� TCnLockObject ��
*           2002.09.10 V1.2
*               �޸�TCnComponent���ַ���
*           2002.07.09 V1.1
*               ������������
*           2002.04.08 V1.0
*               ����TCnComponent�������
*           2002.01.11 V0.01Demo
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, TypInfo;

type

//==============================================================================
// �����߳�ͬ���Ķ�����
//==============================================================================

{ TCnLockObject }

  TCnLockObject = class (TObject)
  {* �����߳�ͬ���Ķ�����}
  private
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
    function GetLocking: Boolean;
  protected
    property LockCount: Integer read FLockCount;
    {* ��ǰLock������ֻ������}
  public
    constructor Create;
    {* �����������ڲ���һ�������ʵ��}
    destructor Destroy; override;
    procedure Lock;
    {* �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��}
    function TryLock: Boolean;
    {* �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ١�
       ��������棬�����ڲ�����ɺ����UnLock�ͷ���}
    procedure Unlock;
    {* �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��}
    property Locking: Boolean read GetLocking;
    {* ȡ��ǰ����״̬}
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TPersistent ��
//==============================================================================

{ TCnAssignablePersistent }

  TCnAssignablePersistent = class(TPersistent)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollectionItem ��
//==============================================================================

{ TCnAssignableCollectionItem }

  TCnAssignableCollectionItem = class(TCollectionItem)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollection ��
//==============================================================================

{ TCnAssignableCollection }

  TCnAssignableCollection = class(TCollection)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ������֪ͨ���̰߳�ȫ�ĳ־�����
//==============================================================================

{ TCnPersistent }

  TCnPersistent = class(TPersistent)
  {* ������֪ͨ���̰߳�ȫ�ĳ־�����}
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TPersistent;
    FLockObject: TCnLockObject;
    function GetLocking: Boolean;
    function GetLockObject: TCnLockObject;
  protected
    function GetOwner: TPersistent; override;
    procedure Changing; virtual;
    {* �������ݿ�ʼ���£�������¼���Ϊ0������OnChanging�¼���������}
    procedure Changed; virtual;
    {* ���������ѱ����������¼���Ϊ0������OnChange�¼���������}

    procedure SetUpdating(Updating: Boolean); virtual;
    {* ����״̬������̣������ء�
       Ĭ��Ϊ��ʼ����ʱ����Changing������ʱ����Changed}
    function IsUpdating: Boolean;
    {* ��ǰ���¼����Ƿ����0�����ڸ��£�}

    procedure OnChildChanging(Sender: TObject); virtual;
    {* �����Կ�ʼ�����¼�������̣�����Ϊ�������ݸ�TCnPersistent.Create����
       Ĭ��Ϊ����OnChanging�¼���������}
    procedure OnChildChange(Sender: TObject); virtual;
    {* �������ѱ���¼�������̣�����Ϊ�������ݸ�TCnPersistent.Create����
       Ĭ��Ϊ����OnChange�¼���������}

    property Owner: TPersistent read FOwner write FOwner;
    {* ����������� }
    property LockObject: TCnLockObject read GetLockObject;
    {* �߳�ͬ������ }
  public
    constructor Create; overload; virtual;
    {* �����������ڲ���һ�������ʵ����������}
    constructor Create(AOwner: TPersistent); overload;
    {* ������������Ϊʵ���������ߣ�����ֱ�ӻ��Ӱ���TCollection������Ҫ��Ϊ
       published ����ʱʹ��}
    constructor Create(ChangeProc: TNotifyEvent); overload;
    {* ���������������ڸ�OnChange�¼�ָ��һ����ʼֵ}
    constructor Create(ChangingProc, ChangeProc: TNotifyEvent); overload;
    {* ���������������ڸ�OnChanging��OnChange�¼�ָ��һ����ʼֵ}
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    {* ��ʼ���£������ǰ���¼���Ϊ0���Զ�����Changing�����������ء�
       �ڶԳ������Խ����޸�ʱ����ø÷�����ע�������EndUpdate�ɶ�ʹ��}
    procedure EndUpdate; virtual;
    {* �������£������ǰ���¼���Ϊ0���Զ�����Change�����������ء�
       �ڶԳ��������޸ĺ�����ø÷�����ע�������BeginUpdate�ɶ�ʹ��}

    procedure Lock;
    {* �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��}
    function TryLock: Boolean;
    {* �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ١�
       ��������棬�����ڲ�����ɺ����UnLock�ͷ���}
    procedure Unlock;
    {* �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��}

    property Locking: Boolean read GetLocking;
    {* ȡ��ǰ����״̬}
  published
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    {* ����ʼ�����¼�}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {* ���������ѱ���¼�}
  end;

//==============================================================================
// ��Enabled�ĸ���֪ͨ�־�����
//==============================================================================

{ TCnEnabledPersistent }

  TCnEnabledPersistent = class(TCnPersistent)
  {* ��Enabled�ĸ���֪ͨ�־�����}
  private
    FEnabled: Boolean;
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetUpdating(Updating: Boolean); override;
  public
    constructor Create; override;
    {* �����������ڲ���һ�������ʵ��}
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* Enabled���ԣ����Ϊ�٣���Changing��Changed�����ĵ��ý������������¼�}
  end;

//==============================================================================
// ���������������
//==============================================================================

{ TCnComponent }

  TCnCopyright = type string;

  TCnComponent = class(TComponent)
  {* CnPack�������������}
  private
    FAbout: TCnCopyright;
    procedure SetAbout(const Value: TCnCopyright);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); virtual;
      abstract;
    {* ȡ�����Ϣ�������ṩ�����˵���Ͱ�Ȩ��Ϣ�����󷽷����������ʵ�֡�
     |<PRE>
       var AName: string      - ������ƣ�������֧�ֱ��ػ����ַ���
       var Author: string     - ������ߣ�����ж�����ߣ��÷ֺŷָ�
       var Email: string      - ����������䣬����ж�����ߣ��÷ֺŷָ�
       var Comment:           - ���˵����������֧�ֱ��ػ������з����ַ���
     |</PRE>}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property About: TCnCopyright read FAbout write SetAbout stored False;
    {* ����汾���ԣ����������ʹ��}
  end;

//==============================================================================
// ��ʵ���ӿڶ��������
//==============================================================================

{ TSingletonInterfacedObject }

  TSingletonInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties:
  Boolean = True);

implementation

uses
  CnConsts;

type
  TPersistentHack = class(TPersistent);

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties: 
  Boolean = True);
var
  Stream: TMemoryStream;
  Reader: TReader;
  Writer: TWriter;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  if Source is Dest.ClassType then
  begin
    // ʹ�� RTTI ����֤��ֵ���� published ���ԣ������ܴ���ֵΪ Default �����ԣ�
    Count := GetPropList(Dest.ClassInfo, tkProperties - [tkArray, tkRecord,
      tkInterface], nil);
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropList(Source.ClassInfo, tkProperties - [tkArray, tkRecord,
        tkInterface], @PropList^[0]);
      for PropIdx := 0 to Count - 1 do
      begin
        PropInfo := PropList^[PropIdx];
        case PropInfo^.PropType^^.Kind of
          tkInteger, tkChar, tkWChar, tkClass, tkEnumeration, tkSet:
            SetOrdProp(Dest, PropInfo, GetOrdProp(Source, PropInfo));
          tkFloat:
            SetFloatProp(Dest, PropInfo, GetFloatProp(Source, PropInfo));
          tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
            SetStrProp(Dest, PropInfo, GetStrProp(Source, PropInfo));
          tkVariant:
            SetVariantProp(Dest, PropInfo, GetVariantProp(Source, PropInfo));
          tkInt64:
            SetInt64Prop(Dest, PropInfo, GetInt64Prop(Source, PropInfo));
          tkMethod:
            SetMethodProp(Dest, PropInfo, GetMethodProp(Source, PropInfo));
        end;
      end;
    finally
      FreeMem(PropList);
    end;

    // ʹ�����������Զ��������
    if UseDefineProperties then
    begin
      Stream := nil;
      Reader := nil;
      Writer := nil;
      try
        Stream := TMemoryStream.Create;
        Writer := TWriter.Create(Stream, 4096);
        TPersistentHack(Source).DefineProperties(Writer);
        Writer.FlushBuffer;
        Stream.Position := 0;
        Reader := TReader.Create(Stream, 4096);
        TPersistentHack(Dest).DefineProperties(Reader);
      finally
        FreeAndNil(Reader);
        FreeAndNil(Writer);
        FreeAndNil(Stream);
      end;
    end;
  end;
end;

//==============================================================================
// ֧���̰߳�ȫ�Ļ�����
//==============================================================================

var
  CounterLock: TRTLCriticalSection;

{ TCnLockObject }

// ��ʼ��
constructor TCnLockObject.Create;
begin
  inherited;
  InitializeCriticalSection(FLock); // ��ʼ���ٽ���
end;

// �ͷ�
destructor TCnLockObject.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

// ���Խ����ٽ���������Ѽ������� False��
function TCnLockObject.TryLock: Boolean;
begin
  EnterCriticalSection(CounterLock);
  try
    Result := FLockCount = 0;
    if Result then Lock;
  finally
    LeaveCriticalSection(CounterLock);
  end;
end;

// ����
procedure TCnLockObject.Lock;
begin
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
end;

// �ͷ���
procedure TCnLockObject.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
end;

function TCnLockObject.GetLocking: Boolean;
begin
  Result := FLockCount > 0;
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TPersistent ��
//==============================================================================

{ TCnAssignablePersistent }

procedure TCnAssignablePersistent.Assign(Source: TPersistent);
begin
  if Source is ClassType then 
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollectionItem ��
//==============================================================================

{ TCnAssignableCollectionItem }

procedure TCnAssignableCollectionItem.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollection ��
//==============================================================================

{ TCnAssignableCollection }

procedure TCnAssignableCollection.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end;
  inherited Assign(Source);
end;

//==============================================================================
// ������֪ͨ���̰߳�ȫ�ĳ־�����
//==============================================================================

{ TCnPersistent }

// ��ʼ���������أ�
constructor TCnPersistent.Create;
begin
  inherited;
  FUpdateCount := 0;
end;

// ��ʼ��������Ϊʵ����������
constructor TCnPersistent.Create(AOwner: TPersistent);
begin
  Create;
  FOwner := AOwner;
end;

// ��ʼ��������Ϊ����֪ͨ�¼�
constructor TCnPersistent.Create(ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChange := ChangeProc;
end;

// ��ʼ��������Ϊ����֪ͨ�¼�
constructor TCnPersistent.Create(ChangingProc, ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChanging := ChangingProc;
  FOnChange := ChangeProc;
end;

destructor TCnPersistent.Destroy;
begin
  if Assigned(FLockObject) then
    FLockObject.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// ����֪ͨ����
//------------------------------------------------------------------------------

// ��ʼ����
procedure TCnPersistent.BeginUpdate;
begin
  if not IsUpdating then SetUpdating(True); // ��ʼ����
  Inc(FUpdateCount);
end;

// ��������
procedure TCnPersistent.EndUpdate;
begin                         // Assert����Ҫ���ػ�
  Assert(FUpdateCount > 0, 'Unpaired TCnPersistent.EndUpdate');
  Dec(FUpdateCount);
  if not IsUpdating then SetUpdating(False);
end;

// ���ڱ��
procedure TCnPersistent.Changing;
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Self);
end;

// �������
procedure TCnPersistent.Changed;
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Self);
end;

// ȡ������
function TCnPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ���ڸ���
function TCnPersistent.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// ����״̬�������
procedure TCnPersistent.SetUpdating(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

// �ӵ�λ���
procedure TCnPersistent.OnChildChanging(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Sender);
end;

// �ӵ�λ�ѱ��
procedure TCnPersistent.OnChildChange(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Sender);
end;

//------------------------------------------------------------------------------
// �̰߳�ȫ������
//------------------------------------------------------------------------------

// �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��
procedure TCnPersistent.Lock;
begin
  LockObject.Lock;
end;

// �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ�
function TCnPersistent.TryLock: Boolean;
begin
  Result := LockObject.TryLock;
end;

// �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��
procedure TCnPersistent.Unlock;
begin
  LockObject.Unlock;
end;

// Locking ���Զ�����
function TCnPersistent.GetLocking: Boolean;
begin
  Result := LockObject.GetLocking;
end;

// LockObject ���Զ�������������Ҫʱ�����ڲ�����
function TCnPersistent.GetLockObject: TCnLockObject;
begin
  if not Assigned(FLockObject) then
    FLockObject := TCnLockObject.Create;
  Result := FLockObject;
end;

//==============================================================================
// ��Enabled�ĸ���֪ͨ�־�����
//==============================================================================

{ TCnEnabledPersistent }

// ��ֵ
procedure TCnEnabledPersistent.Assign(Source: TPersistent);
begin
  if Source is TCnEnabledPersistent then
    FEnabled := TCnEnabledPersistent(Source).FEnabled
  else
    inherited Assign(Source);
end;

// ����֪ͨ
procedure TCnEnabledPersistent.SetUpdating(Updating: Boolean);
begin
  if FEnabled then            // ���������֪ͨ
    inherited SetUpdating(Updating); 
end;

// ����
constructor TCnEnabledPersistent.Create;
begin
  inherited Create;
  FEnabled := False;
end;

// ���ò���
procedure TCnEnabledPersistent.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := True;         // ����֪ͨ
    Changed;
    FEnabled := Value;
  end;
end;

//==============================================================================
// ���������������
//==============================================================================

{ TCnComponent }

// ��ʼ��
constructor TCnComponent.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := SCnPackAbout;
end;

// ���ù�������
procedure TCnComponent.SetAbout(const Value: TCnCopyright);
begin
  // ������
end;

//==============================================================================
// ��ʵ���ӿڶ��������
//==============================================================================

{ TSingletonInterfacedObject }

function TSingletonInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TSingletonInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

initialization
  InitializeCriticalSection(CounterLock);

finalization
  DeleteCriticalSection(CounterLock);

end.

