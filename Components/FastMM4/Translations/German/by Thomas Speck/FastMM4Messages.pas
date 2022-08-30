{

Fast Memory Manager: Messages

German Translation by Thomas Speck (thomas.speck@tssoft.de).

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
  UnknownClassNameMsg = 'Unbekannt';
  {Memory dump message}
  MemoryDumpMsg = #13#10#13#10'Aktueller Speicherauszug von 256 Bytes, beginnend ab Zeigeradresse ';
  {Block Error Messages}
  BlockScanLogHeader = 'Allocated block logged by LogAllocatedBlocksToFile. The size is: ';
  ErrorMsgHeader = 'FastMM hat einen Fehler entdeckt w�hrend einem / einer';
  GetMemMsg = 'GetMem';
  FreeMemMsg = 'FreeMem';
  ReallocMemMsg = 'ReallocMem';
  BlockCheckMsg = 'Freien Block-Scan';
  OperationMsg = ' Operation. ';
  BlockHeaderCorruptedMsg = 'Der Block-Beginn ist defekt. ';
  BlockFooterCorruptedMsg = 'Das Block-Ende ist defekt. ';
  FreeModifiedErrorMsg = 'FastMM entdeckte einen Block, der nach der Freigabe ver�ndert wurde. ';
  FreeModifiedDetailMsg = #13#10#13#10'Modified byte offsets (and lengths): ';
  DoubleFreeErrorMsg = 'Es wurde versucht, einen unbelegten Block freizugeben bzw. zu belegen.';
  PreviousBlockSizeMsg = #13#10#13#10'Die vorherige Blockgr��e war: ';
  CurrentBlockSizeMsg = #13#10#13#10'Die Blockgr��e ist: ';
  PreviousObjectClassMsg = #13#10#13#10'Der Block wurde vorher f�r eine Objektklasse benutzt: ';
  CurrentObjectClassMsg = #13#10#13#10'Der Block wird momentan f�r eine Objektklasse benutzt ';
  PreviousAllocationGroupMsg = #13#10#13#10'The allocation group was: ';
  PreviousAllocationNumberMsg = #13#10#13#10'The allocation number was: ';
  CurrentAllocationGroupMsg = #13#10#13#10'The allocation group is: ';
  CurrentAllocationNumberMsg = #13#10#13#10'The allocation number is: ';
  BlockErrorMsgTitle = 'Speicherfehler entdeckt';
  VirtualMethodErrorHeader = 'FastMM hat einen Versuch entdeckt, eine virtuelle Methode eines freigegebenen Objektes aufzurufen. Eine Schutzverletzung wird nun aufgerufen, um die aktuelle Operation abzubrechen.';
  InterfaceErrorHeader = 'FastMM hat einen Versuch entdeckt, ein Interface eines freigegebenen Objektes aufzurufen. Eine Schutzverletzung wird nun aufgerufen, um die aktuelle Operation abzubrechen.';
  BlockHeaderCorruptedNoHistoryMsg = ' Ungl�cklicherweise wurde der Block-Beginn besch�digt, so da� keine Historie verf�gbar ist.';
  FreedObjectClassMsg = #13#10#13#10'Freigegebene Objekt-Klasse: ';
  VirtualMethodName = #13#10#13#10'Virtuelle Methode: ';
  VirtualMethodOffset = 'Offset +';
  VirtualMethodAddress = #13#10#13#10'Adresse der virtuellen Methode: ';
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
  AlreadyInstalledMsg = 'FastMM4 ist installiert.';
  AlreadyInstalledTitle = 'Schon installiert.';
  OtherMMInstalledMsg = 'FastMM4 kann nicht installiert werden, weil ein schon ein anderer '
    + 'Memory Manager installiert wurde.'#13#10'Wenn Sie FastMM4 benutzen wollen, '
    + 'dann vergewissern Sie sich, da� FastMM4.pas die allererste Unit in der "uses"'
    + #13#10'Sektion Ihrer Projektdatei ist.';
  OtherMMInstalledTitle = 'Kann FastMM4 nicht installieren - Ein anderer Memory Manager ist schon installiert.';
  MemoryAllocatedMsg = 'FastMM4 kann nicht installiert werden, weil schon Speicher'
    + 'durch den Default Memory Manager belegt wurde.'#13#10'FastMM4.pas MUSS '
    + 'die allererste Unit in Ihrer Projektdatei sein, sonst wird der Speicher '
    + 'durch den Default Memory Manager belegt, bevor FastMM4 die Kontrolle �bernimmt. '
    + #13#10#13#10'Wenn Sie ein Programm benutzen, welches Exceptions abf�ngt '
    + 'z.B. MadExcept (oder ein anderes Tool, das die Reihenfolge der Unit Initialisierung '
    + 'ver�ndert),'#13#10'dann gehen Sie in seine Konfiguration und stellen Sie sicher, da� '
    + 'FastMM4.pas Unit vor jeder anderen Unit initialisiert wird.';
  MemoryAllocatedTitle = 'Kann FastMM4nicht installieren - Speicher wurde schon belegt.';
  {Leak checking messages}
  LeakLogHeader = 'Ein Speicherblock hat Speicher verloren. Die Gr��e ist: ';
  LeakMessageHeader = 'Diese Anwendung hat Speicher verloren. ';
  SmallLeakDetail = 'Die Gr��en von kleinen Speicherbl�cken, die verlorengegangen sind, betragen'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (ausgenommen erwartete Speicherlecks, die durch Zeiger registriert wurden)'
{$endif}
    + ':'#13#10;
  LargeLeakDetail = 'Die Gr��en von mittleren und gro�en Speicherbl�cken, die verlorengegangen sind, betragen'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (ausgenommen erwartete Speicherlecks, die durch Zeiger registriert wurden)'
{$endif}
    + ': ';
  BytesMessage = ' Bytes: ';
  AnsiStringBlockMessage = 'AnsiString';
  UnicodeStringBlockMessage = 'UnicodeString';
  LeakMessageFooter = #13#10
{$ifndef HideMemoryLeakHintMessage}
    + #13#10'Notiz: '
  {$ifdef RequireIDEPresenceForLeakReporting}
    + 'Diese �berpr�fung auf Speicherlecks wird nur durchgef�hrt, wenn Delphi auf dem selben Computer gestartet ist. '
  {$endif}
  {$ifdef FullDebugMode}
    {$ifdef LogMemoryLeakDetailToFile}
    + 'Speicherleck-Details werden in eine Textdatei geschrieben, die sich im selben Verzeichnis wie diese Anwendung befindet. '
    {$else}
    + 'Aktiviere "LogMemoryLeakDetailToFile", um eine detaillierte Log-Datei zu erhalten, die Details zu Speicherlecks enth�lt. '
    {$endif}
  {$else}
    + 'Um eine Log-Datei zu erhalten, die Details zu Speicherlecks enth�lt, aktivieren Sie "FullDebugMode" und "LogMemoryLeakDetailToFile" in der Options-Datei. '
  {$endif}
    + 'Um diese Speicherleck-�berpr�fung abzuschalten, kommentieren Sie "EnableMemoryLeakReporting" aus.'#13#10
{$endif}
    + #0;
  LeakMessageTitle = 'Speicherleck entdeckt';
{$ifdef UseOutputDebugString}
  FastMMInstallMsg = 'FastMM wurde installiert.';
  FastMMInstallSharedMsg = 'Benutzung einer existierenden Instanz von FastMM wurde gestartet.';
  FastMMUninstallMsg = 'FastMM wurde deinstalliert.';
  FastMMUninstallSharedMsg = 'Benutzung einer existierenden Instanz von FastMM wurde gestoppt.';
{$endif}
{$ifdef DetectMMOperationsAfterUninstall}
  InvalidOperationTitle = 'MM Operation nach der Deinstallierung.';
  InvalidGetMemMsg = 'FastMM hat einen GetMem-Aufruf nach der Deinstallation von FastMM entdeckt.';
  InvalidFreeMemMsg = 'FastMM hat einen FreeMem-Aufruf nach der Deinstallation von FastMM entdeckt.';
  InvalidReallocMemMsg = 'FastMM hat einen ReAllocMem-Aufruf nach der Deinstallation von FastMM entdeckt.';
  InvalidAllocMemMsg = 'FastMM hat einen AllocMem-Aufruf nach der Deinstallation von FastMM entdeckt.';
{$endif}

implementation

end.

