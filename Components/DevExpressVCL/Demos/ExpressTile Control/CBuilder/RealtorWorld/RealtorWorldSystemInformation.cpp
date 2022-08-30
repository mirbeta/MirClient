//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldSystemInformation.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxImage"
#pragma link "dxGDIPlusClasses"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxFilter"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxProgressBar"
#pragma link "cxSpinEdit"
#pragma link "cxStyles"

#include "TlHelp32.hpp"
#include "PsAPI.hpp"
#include "Registry.hpp"

#pragma resource "*.dfm"
TfrmSystemInformation *frmSystemInformation;
//---------------------------------------------------------------------------
__fastcall TfrmSystemInformation::TfrmSystemInformation(TComponent* Owner)
	: TfrmBase(Owner)
{
	lbComputerName->Caption = GetMachineName();
	QuerySystemInfo();
    tmRefeshTimer(NULL);
}
//---------------------------------------------------------------------------
String TfrmSystemInformation::GetMachineName()
{
	DWORD ABufferSize;
	char  ABuffer[MAX_PATH + 1];

	ABufferSize = MAX_PATH + 1;
	GetComputerName(&ABuffer[0], &ABufferSize);

	return (String)ABuffer;
}
//---------------------------------------------------------------------------
DWORD TfrmSystemInformation::GetProcessMemoryUsage(THandle AProcessID)
{
	DWORD AResult;
	TProcessMemoryCounters ACounters;
	HANDLE AProcessHandle;

	AResult = 0;
	AProcessHandle = OpenProcess(PROCESS_QUERY_INFORMATION, true, AProcessID);
	if (AProcessHandle)
	{
		ZeroMemory(&ACounters, sizeof(ACounters));
		ACounters.cb = sizeof(ACounters);
		if (GetProcessMemoryInfo(AProcessHandle, &ACounters, sizeof(ACounters)))
			AResult = ACounters.WorkingSetSize;
		CloseHandle(AProcessHandle);
	}
	return AResult;
}
//---------------------------------------------------------------------------
void TfrmSystemInformation::QueryMemoryUsage()
{
	TMemoryStatus AMemStatus;

	ZeroMemory(&AMemStatus, sizeof(AMemStatus));
	AMemStatus.dwLength = sizeof(AMemStatus);
	GlobalMemoryStatus(&AMemStatus);
	pbMemoryUsage->Position = 100.0 * (AMemStatus.dwTotalPhys - AMemStatus.dwAvailPhys) / AMemStatus.dwTotalPhys;
	pbMemoryUsage->Properties->Text = FormatFloat("0.00", AMemStatus.dwAvailPhys / (1024.0 * 1024.0 * 1024.0)) + " GB Free";
}
//---------------------------------------------------------------------------
void TfrmSystemInformation::QuerySystemInfo()
{
	TRegistry *ARegistry;

	ARegistry = new TRegistry();
	ARegistry->RootKey = HKEY_LOCAL_MACHINE;
	if (ARegistry->OpenKeyReadOnly("SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"))
	{
		lbWindowsInfo->Caption = ARegistry->ReadString("ProductName") + " " + ARegistry->ReadString("CSDVersion");
		ARegistry->CloseKey();
	};
	if (ARegistry->OpenKeyReadOnly("HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0"))
	{
		lbProcessorInfo->Caption = ARegistry->ReadString("ProcessorNameString");
		ARegistry->CloseKey();
	}
	delete ARegistry;
}
//---------------------------------------------------------------------------
void __fastcall TfrmSystemInformation::tmRefeshTimer(TObject *Sender)
{
	tagPROCESSENTRY32 AProcessInfo;
	int ARecordIndex;
	HANDLE ASnapshot;

	ASnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	try
	{
		ZeroMemory(&AProcessInfo, sizeof(AProcessInfo));
		AProcessInfo.dwSize = sizeof(AProcessInfo);
		cxGrid1TableView1->DataController->BeginFullUpdate();
		try
		{
			cxGrid1TableView1->DataController->RecordCount = 0;
			if (Process32First(ASnapshot, &AProcessInfo))
			{
				do
				{
					ARecordIndex = cxGrid1TableView1->DataController->RecordCount;
					cxGrid1TableView1->DataController->RecordCount += 1;
					cxGrid1TableView1->DataController->SetValue(ARecordIndex, cxGrid1TableView1Column1->Index, AProcessInfo.th32ProcessID);
					cxGrid1TableView1->DataController->SetValue(ARecordIndex, cxGrid1TableView1Column2->Index, (String)AProcessInfo.szExeFile);
					cxGrid1TableView1->DataController->SetValue(ARecordIndex, cxGrid1TableView1Column3->Index, GetProcessMemoryUsage(AProcessInfo.th32ProcessID) / 1024);
				}
				while (Process32Next(ASnapshot, &AProcessInfo));
			}
		}
		__finally
		{
			cxGrid1TableView1->DataController->EndFullUpdate();
		}
	}
	__finally
	{
		CloseHandle(ASnapshot);
	}
	QueryMemoryUsage();
}
//---------------------------------------------------------------------------

