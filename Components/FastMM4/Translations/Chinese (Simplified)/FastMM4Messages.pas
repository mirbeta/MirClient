{

Fast Memory Manager: Messages

Simplified Chinese translation by JiYuan Xie.

}

unit FastMM4Messages;

interface

{$Include FastMM4Options.inc}

const
  {The name of the debug info support DLL}
  FullDebugModeLibraryName = 'FastMM_FullDebugMode.dll';
  {Event log strings}
  LogFileExtension = '_MemoryManager_EventLog.txt'#0;
  CRLF = #13#10;
  EventSeparator = '--------------------------------';
  {Class name messages}
  UnknownClassNameMsg = 'δ֪';
  {Memory dump message}
  MemoryDumpMsg = #13#10#13#10'��ָ����ָ��ַ��ʼ, 256�ֽڵ��ڴ浱ǰ������ ';
  {Block Error Messages}
  BlockScanLogHeader = '�� LogAllocatedBlocksToFile ��¼���ѷ����ڴ��. ��С��: ';
  ErrorMsgHeader = 'FastMM �Ѽ�⵽һ������, ��ʱ���ڽ��� ';
  GetMemMsg = 'GetMem';
  FreeMemMsg = 'FreeMem';
  ReallocMemMsg = 'ReallocMem';
  BlockCheckMsg = 'ɨ�������ڴ��';
  OperationMsg = ' ����. ';
  BlockHeaderCorruptedMsg = '�ڴ��ͷ�������ѱ��ƻ�. ';
  BlockFooterCorruptedMsg = '�ڴ��β�������ѱ��ƻ�. ';
  FreeModifiedErrorMsg = 'FastMM ��⵽�����ͷ��ڴ�����ݵ��޸�. ';
  FreeModifiedDetailMsg = #13#10#13#10'Modified byte offsets (and lengths): ';
  DoubleFreeErrorMsg = '��ͼ�ͷ�/���·���һ����δ������ڴ��.';
  PreviousBlockSizeMsg = #13#10#13#10'�ϴ�ʹ��ʱ���ڴ���С��: ';
  CurrentBlockSizeMsg = #13#10#13#10'�ڴ��Ĵ�С��: ';
  PreviousObjectClassMsg = #13#10#13#10'���ڴ���ϴα�����һ������������Ķ���: ';
  CurrentObjectClassMsg = #13#10#13#10'���ڴ�鵱ǰ������һ������������Ķ���: ';
  PreviousAllocationGroupMsg = #13#10#13#10'��������: ';
  PreviousAllocationNumberMsg = #13#10#13#10'���������: ';
  CurrentAllocationGroupMsg = #13#10#13#10'��������: ';
  CurrentAllocationNumberMsg = #13#10#13#10'���������: ';
  BlockErrorMsgTitle = '��⵽�ڴ����';
  VirtualMethodErrorHeader = 'FastMM ��⵽�����ͷŶ�����鷽���ĵ���. һ�����ʳ�ͻ�쳣���ڽ�����������ֹ��ǰ�Ĳ���.';
  InterfaceErrorHeader = 'FastMM ��⵽�����ͷŶ���Ľӿڵ�ʹ��. һ�����ʳ�ͻ�쳣���ڽ�����������ֹ��ǰ�Ĳ���.';
  BlockHeaderCorruptedNoHistoryMsg = ' ���ҵ�, �����ڴ��ͷ���������ѱ��ƻ�, �޷��õ����ڴ���ʹ����ʷ.';
  FreedObjectClassMsg = #13#10#13#10'���ͷŵĶ�����������: ';
  VirtualMethodName = #13#10#13#10'�鷽��: ';
  VirtualMethodOffset = 'ƫ�Ƶ�ַ +';
  VirtualMethodAddress = #13#10#13#10'�鷽���ĵ�ַ: ';
  {Stack trace messages}
  CurrentThreadIDMsg = #13#10#13#10'The current thread ID is 0x';
  CurrentStackTraceMsg = ', and the stack trace (return addresses) leading to this error is:';
  ThreadIDPrevAllocMsg = #13#10#13#10'This block was previously allocated by thread 0x';
  ThreadIDAtAllocMsg = #13#10#13#10'This block was allocated by thread 0x';
  ThreadIDAtFreeMsg = #13#10#13#10'The block was previously freed by thread 0x';
  ThreadIDAtObjectAllocMsg = #13#10#13#10'The object was allocated by thread 0x';
  ThreadIDAtObjectFreeMsg = #13#10#13#10'The object was subsequently freed by thread 0x';
  StackTraceMsg = ', and the stack trace (return addresses) at the time was:';
  {Installation Messages}
  AlreadyInstalledMsg = 'FastMM4 �Ѿ�����װ';
  AlreadyInstalledTitle = '�Ѿ�����';
  OtherMMInstalledMsg = 'FastMM4 �޷�����װ, ��Ϊ�����������ڴ�������������а�װ.'
    + #13#10'�������ʹ��FastMM4, ��ȷ��������Ŀ�� .dpr �ļ��� "uses" ������, '
    + #13#10'FastMM4.pas �ǵ�һ����ʹ�õĵ�Ԫ.';
  OtherMMInstalledTitle = '�޷���װFastMM4 - �����ڴ���������ȱ���װ';
  MemoryAllocatedMsg = 'FastMM4 �޷���װ, ��Ϊ��ǰ��ͨ��Ĭ���ڴ�������������ڴ�.'
    + #13#10'FastMM4.pas ����������Ŀ�� .dpr �ļ��е�һ����ʹ�õĵ�Ԫ, ���������'
    + #13#10'FastMM4 �õ�����Ȩ֮ǰ, Ӧ�ó����Ѿ�ͨ��Ĭ���ڴ�������������ڴ�.'
    + #13#10#13#10'�����ʹ�����쳣��׽����, �� MadExcept(���κν��޸ĵ�Ԫ��ʼ��˳��Ĺ���),'
    + #13#10'�뵽��������ҳ��,ȷ�� FastMM4.pas ��Ԫ���κ�������Ԫ֮ǰ����ʼ��.';
  MemoryAllocatedTitle = '�޷���װ FastMM4 - ֮ǰ�Ѿ��������ڴ�';
  {Leak checking messages}
  LeakLogHeader = 'һ���ڴ����й¶. ��С��: ';
  LeakMessageHeader = '���Ӧ�ó�������ڴ�й¶. ';
  SmallLeakDetail = 'С�ڴ���й¶��'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (�������Ѱ�ָ��ע���Ԥ֪й¶)'
{$endif}
    + ':'#13#10;
  LargeLeakDetail = '��й¶���еȼ����ڴ��Ĵ�С��'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (�������Ѱ�ָ��ע���Ԥ֪й¶)'
{$endif}
    + ': ';
  BytesMessage = ' �ֽ�: ';
  AnsiStringBlockMessage = 'AnsiString';
  UnicodeStringBlockMessage = 'UnicodeString';
  LeakMessageFooter = #13#10
{$ifndef HideMemoryLeakHintMessage}
    + #13#10'ע��: '
  {$ifdef RequireIDEPresenceForLeakReporting}
    + 'ֻ�е� Delphi ͬʱ������ͬһ�������ʱ�Ż�����ڴ�й¶���. '
  {$endif}
  {$ifdef FullDebugMode}
    {$ifdef LogMemoryLeakDetailToFile}
    + '�ڴ�й¶����ϸ��Ϣ�Ѿ�����¼���뱾Ӧ�ó���ͬһĿ¼�µ�һ���ı��ļ���. '
    {$else}
    + '������ "LogMemoryLeakDetailToFile" �������뿪���Եõ�һ�����������ڴ�й¶����ϸ��Ϣ����־�ļ�. '
    {$endif}
  {$else}
    + 'Ҫ�õ�һ�����������ڴ�й¶����ϸ��Ϣ����־�ļ�, ������ "FullDebugMode" �� "LogMemoryLeakDetailToFile" �������뿪��. '
  {$endif}
    + 'Ҫ��ֹ�ڴ�й¶���, ��ر� "EnableMemoryLeakReporting" �������뿪��.'#13#10
{$endif}
    + #0;
  LeakMessageTitle = '��⵽�ڴ�й¶';
{$ifdef UseOutputDebugString}
  FastMMInstallMsg = 'FastMM �ѱ���װ.';
  FastMMInstallSharedMsg = '������һ���Ѵ��ڵ� FastMM ʵ��.';
  FastMMUninstallMsg = 'FastMM �ѱ�ж��.';
  FastMMUninstallSharedMsg = '��ֹͣ����һ���Ѵ��ڵ� FastMM ʵ��.';
{$endif}
{$ifdef DetectMMOperationsAfterUninstall}
  InvalidOperationTitle = 'ж��֮������ MM ����.';
  InvalidGetMemMsg = 'FastMM ��⵽�� FastMM ��ж��֮������� GetMem.';
  InvalidFreeMemMsg = 'FastMM ��⵽�� FastMM ��ж��֮������� FreeMem.';
  InvalidReallocMemMsg = 'FastMM ��⵽�� FastMM ��ж��֮������� ReallocMem.';
  InvalidAllocMemMsg = 'FastMM ��⵽�� FastMM ��ж��֮������� AllocMem.';
{$endif}

implementation

end.

