//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonNotepadDemoOptions.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TRibbonDemoOptionsForm *RibbonDemoOptionsForm;

BOOL ExecuteRibbonDemoOptions(String *AColorSchemeName,
	TScreenTipOptions *AScreenTipOptions, TRibbonDemoStyle *AStyle,
	TdxRibbonColorSchemeAccent *AColorSchemeAccent)
{
	TRibbonDemoOptionsForm *Form = new TRibbonDemoOptionsForm(NULL);
	PopulateRibbonStyles(Form->cbRibbonStyle->Properties->Items);
	PopulateColorSchemeAccents(Form->cbColorSchemeAccent->Properties->Items);
	Form->LoadOptions(*AColorSchemeName, *AScreenTipOptions, *AStyle, *AColorSchemeAccent);
	BOOL Result = Form->ShowModal() == mrOk;
	if (Result)
	  Form->SaveOptions(AColorSchemeName, AScreenTipOptions, AStyle, AColorSchemeAccent);
	delete Form;
	return Result;
}

TdxRibbonStyle RibbonDemoStyleToRibbonStyle(TRibbonDemoStyle ADemoStyle)
{
	switch (ADemoStyle)
	{
		case rdsOffice2007:
			return rs2007;
		case rdsOffice2010:
			return rs2010;
		case rdsOffice2013:
			return rs2013;
		case rdsOffice2016:
			return rs2016;
		case rdsOffice2016Tablet:
			return rs2016Tablet;
		case rdsOffice2019:
			return rs2019;
		default: // Scenic
			return rs2010;
	}
}

//---------------------------------------------------------------------------
__fastcall TRibbonDemoOptionsForm::TRibbonDemoOptionsForm(TComponent* Owner)
		: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::LoadOptions(String AColorSchemeName,
	TScreenTipOptions AScreenTipOptions, TRibbonDemoStyle AStyle,
	TdxRibbonColorSchemeAccent AColorSchemeAccent)
{
	cbRibbonStyle->ItemIndex = (Integer)AStyle;
	cbRibbonStyleSelect(NULL);

	cbColorScheme->ItemIndex = cbColorScheme->Properties->Items->IndexOf(AColorSchemeName);
	cbColorSchemeAccent->ItemIndex = (Integer)AColorSchemeAccent;
	if (AScreenTipOptions.ShowScreenTips)
		cbScreenTipStyle->ItemIndex = (Integer)(!AScreenTipOptions.ShowDescripitons);
	else
    	cbScreenTipStyle->ItemIndex = 2;
}
//---------------------------------------------------------------------------
void TRibbonDemoOptionsForm::SaveOptions(String *AColorSchemeName,
	TScreenTipOptions *AScreenTipOptions, TRibbonDemoStyle *AStyle,
	TdxRibbonColorSchemeAccent *AColorSchemeAccent)
{
	* AColorSchemeName = cbColorScheme->Text;
	* AStyle = (TRibbonDemoStyle)cbRibbonStyle->ItemIndex;
	* AColorSchemeAccent = (TdxRibbonColorSchemeAccent)cbColorSchemeAccent->ItemIndex;
	AScreenTipOptions->ShowScreenTips = cbScreenTipStyle->ItemIndex != 2;
	AScreenTipOptions->ShowDescripitons = cbScreenTipStyle->ItemIndex == 0;
}
//---------------------------------------------------------------------------
void PopulateColorSchemeAccents(TStrings *AItems)
{
	AItems->BeginUpdate();
	try
	{
		AItems->Clear();
		for (int I = (int)rcsaYellow; I <= (int)rcsaPurple; I++)
		{
			AItems->Add(dxRibbonColorSchemeAccentNames[(TdxRibbonColorSchemeAccent)I]);
		}
	}
	__finally
	{
		AItems->EndUpdate();
	}
}
//---------------------------------------------------------------------------
void PopulateColorSchemes(TStrings *AItems, TdxRibbonStyle ARibbonStyle)
{
	AItems->BeginUpdate();
	try
	{
		AItems->Clear();
		for (int I = 0; I < dxRibbonSkinsManager()->SkinCount; I++)
		{
			TdxCustomRibbonSkin *ASkin = dxRibbonSkinsManager()->Skins[I];
			if (ASkin->Style == ARibbonStyle)
			{
				if (AItems->IndexOf(ASkin->Name) < 0)
					AItems->Add(ASkin->Name);
			}
		}
	}
	__finally
	{
		AItems->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void PopulateRibbonStyles(TStrings *AItems)
{
	AItems->BeginUpdate();
	try
	{
		AItems->Clear();
		AItems->Add("Office 2007");
		AItems->Add("Office 2010");
		AItems->Add("Office 2013");
		AItems->Add("Office 2016");
		AItems->Add("Office 2016 Tablet");
		AItems->Add("Office 2019");
		AItems->Add("Scenic");
	}
	__finally
	{
		AItems->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoOptionsForm::cbRibbonStyleSelect(TObject *Sender)
{
	cbColorSchemeAccent->Enabled = cbRibbonStyle->ItemIndex > 0;
	lblColorSchemeAccent->Enabled = cbRibbonStyle->ItemIndex > 0;

	String ASelectedColorSchemeName = cbColorScheme->Text;
	PopulateColorSchemes(cbColorScheme->Properties->Items, RibbonDemoStyleToRibbonStyle((TRibbonDemoStyle)cbRibbonStyle->ItemIndex));
	cbColorScheme->ItemIndex = Max(0, cbColorScheme->Properties->Items->IndexOf(ASelectedColorSchemeName));
}
//---------------------------------------------------------------------------
