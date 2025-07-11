//----------------------------------------------------------------------------//
/*
 * Art2Tga : Extract pictures from an ART file under the TGA format
 *
 * Copyright (C) 2000-2004  Mathieu Olivier (Version 0.1)
 * Copyright (C) 2019-2025  LeoD
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
//----------------------------------------------------------------------------//

// This work is based on the source code of Kenneth Silverman's BUILD 3D
// engine and utilities. You can find Ken Silverman's official web site
// at "http://www.advsys.net/ken".

// v0.1.1 : DumpPalette(); default to internal Duke Nukem 3D palette
// v0.1.2 : Ion Fury 1.0/1.02 palette, from "MrEsturk"'s palette_editor.dat
// v0.1.3 : More Build games' palettes, moved to "build_games_palettes.h"
// v0.1.4 : More Build games' palettes, Blood palette moved to "*.h"

// Current version of this software
#define VERSION "0.1.4"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* ------------- */
/* --- Types --- */
/* ------------- */

// Boolean
#ifndef __cplusplus  // Standard C++ has already this "bool"
  typedef enum { false, true } bool;
#endif

// Definition of some basic types
// Useful, because sometime we need to be sure of the size of an integer type
// May need to be "#ifdef"ed to fit a particular OS on a particular processor
typedef   signed char  int8;
typedef   signed short int16;
typedef   signed int   int32;
typedef unsigned char  uint8;
typedef unsigned short uint16;
typedef unsigned int   uint32;

// Description of a tile
typedef struct {
  uint16 XSize;     // Tile's width
  uint16 YSize;     // Tile's height
  uint32 AnimData;  // Flags and values for animating the picture
  uint32 Offset;    // Offset in the ART file
} Tile_t;


/* ----------------- */
/* --- Constants --- */
/* ----------------- */

// Maximum number of tiles (taken from Ken's "editart.c")
#define MAX_NB_TILES 9216

// Size of the color palette (256 colors, 3 components for each color)
#define PALETTE_SIZE (256 * 3)

// Animation types
const char* AnimationTypes [4] = {"none", "oscillation", "forward", "backward"};

// Build game shorthands : const char* xyzName
// Build game palettes   : const uint8 xyzPalette [PALETTE_SIZE]
#include "build_games_palettes.h"

/* ------------------------ */
/* --- Global variables --- */
/* ------------------------ */

// ART file
FILE* ArtFile = NULL;

// List of tiles in the ART file
uint32 NbTiles = 0;
uint32 TilesStartNum;             // Number of the first tile
Tile_t TilesList [MAX_NB_TILES];

// Color palette (in TGA format: set of {Blue, Green, Red})
uint8 Palette [PALETTE_SIZE];

// Name of the ART file
const char* ArtFileName = NULL;

// Name of the optional palette file
const char* PalFileName   = "palette.dat";
const char* PalFileNameUc = "PALETTE.DAT";

// Name of the palette dump file
const char* DumpFileName  = "palette.dat.c";

// Build game shorthand
const char* GameShort = NULL;

/* ----------------- */
/* --- Functions --- */
/* ----------------- */

/* ------ Prototypes ------ */

// Dump animation data into "animdata.ini"
static bool DumpAnimationData (void);

// Extract pictures from the ART file
static bool ExtractPictures (void);

// Get a uint16 from a little-endian ordered buffer
static uint16 GetLittleEndianUInt16 (const uint8* Buffer);

// Get a uint32 from a little-endian ordered buffer
static uint32 GetLittleEndianUInt32 (const uint8* Buffer);

// Create the pictures list from the ART header
static bool GetPicturesList (void);

// Load the color palette from the PALETTE.DAT file
static bool LoadPalette (void);

// Set a uint16 into a little-endian ordered buffer
static void SetLittleEndianUInt16 (uint16 Integer, uint8* Buffer);

// Extract the picture at TilesList[TileInd] and save it as PictureName
static bool SpawnTGA (uint32 TileInd, const char* PictureName);

// Dump palette into C file to be copied into this source code
static bool DumpPalette (void);


/* ------ Implementations ------ */

/*
====================
DumpAnimationData

Dump animation data into "animdata.ini"
====================
*/
static bool DumpAnimationData (void)
{
  // Variables
  FILE* AnimDataFile;
  uint32 Ind;

  // Create the INI file
  AnimDataFile = fopen ("animdata.ini", "wt");
  if (AnimDataFile == NULL)
  {
     printf ("Error: can't create file \"animdata.ini\"\n");
     return false;
  }

  printf ("Creating \"animdata.ini\"...");
  fflush (stdout);

  // Print some infos at the beginning of the INI file
  fprintf (AnimDataFile,
          "; This file contains animation data from \"%s\"\n"
          "; Extracted by Art2Tga version " VERSION "\n"
          "\n",
          ArtFileName
  );

  // For each tile...
  for (Ind = 0; Ind < NbTiles; Ind++)
    // If it has animation data...
    if (TilesList[Ind].AnimData != 0)
    {
      // If the tile has animation data, print them first.
      // Note: "AnimationType" should be the only parameter to check, but Blood
      //       seems to put informations in the other animation flags even when
      //       the image is not declared as an animation.
      if (((TilesList[Ind].AnimData >>  0) & 0x3F) != 0 ||
          ((TilesList[Ind].AnimData >>  6) & 0x03) != 0 ||
          ((TilesList[Ind].AnimData >> 24) & 0x0F) != 0
         )
      {
        fprintf (AnimDataFile,
                 "[tile%04u.tga -> tile%04u.tga]\n",
                 Ind + TilesStartNum, Ind + TilesStartNum + (TilesList[Ind].AnimData & 0x3F)
                );
        fprintf (AnimDataFile,
                 "   AnimationType=%s\n",
                 AnimationTypes[(TilesList[Ind].AnimData >> 6) & 0x03]
                );
        fprintf (AnimDataFile,
                 "   AnimationSpeed=%u\n",
                 (TilesList[Ind].AnimData >> 24) & 0x0F
                );

        fprintf (AnimDataFile, "\n");
      }

      fprintf (AnimDataFile, "[tile%04u.tga]\n", Ind + TilesStartNum);

      fprintf (AnimDataFile,
               "   XCenterOffset=%d\n",
               (int8)((TilesList[Ind].AnimData >>  8) & 0xFF)
              );
      fprintf (AnimDataFile,
               "   YCenterOffset=%d\n",
               (int8)((TilesList[Ind].AnimData >> 16) & 0xFF)
              );
      fprintf (AnimDataFile,
               "   OtherFlags=%u\n",
               // Those flags are supposed to be useless in the ART format,
               // but Blood seems to use them:
               TilesList[Ind].AnimData >> 28
              );

      fprintf (AnimDataFile, "\n");
    }

  fclose (AnimDataFile);
  printf (" done\n\n");
  return true;
}


/*
====================
ExtractPictures

Extract pictures from the ART file
====================
*/
static bool ExtractPictures (void)
{
  // Variables
  uint32 Ind;
  char ImageFileName [13];

  // A little counter...
  printf ("Extracting pictures:    0");
  fflush (stdout);

  for (Ind = 0; Ind < NbTiles; Ind++)
  {
    // Updating counter
    printf ("\b\b\b\b%4u", Ind);
    fflush (stdout);

    sprintf (ImageFileName, "tile%04u.tga", Ind + TilesStartNum);
    SpawnTGA (Ind, ImageFileName);
  }

  printf ("\b\b\b\bdone\n\n");
  return true;
}


/*
====================
GetLittleEndianUInt16

Get a uint16 from a little-endian ordered buffer
====================
*/
static uint16 GetLittleEndianUInt16 (const uint8* Buffer)
{
  return (uint16)(Buffer[0] | (Buffer[1] << 8));
}


/*
====================
GetLittleEndianUInt32

Get a uint32 from a little-endian ordered buffer
====================
*/
static uint32 GetLittleEndianUInt32 (const uint8* Buffer)
{
  return Buffer[0] | (Buffer[1] << 8) | (Buffer[2] << 16) | (Buffer[3] << 24);
}


/*
====================
GetPicturesList

Create the pictures list from the ART header
====================
*/
static bool GetPicturesList (void)
{
  // Variables
  uint8 Buffer [MAX_NB_TILES * 4];
  uint32 Version, TilesEndNum;
  uint32 Ind;
  size_t CrtOffset;

  // Read the first part of the header
  if (fread (Buffer, 1, 16, ArtFile) != 16)
  {
    printf ("Error: invalid ART file: not enough header data\n");
    return false;
  }
  Version       = GetLittleEndianUInt32 (&Buffer[ 0]);
  NbTiles       = GetLittleEndianUInt32 (&Buffer[ 4]);//useless; overwr. below
  TilesStartNum = GetLittleEndianUInt32 (&Buffer[ 8]);
  TilesEndNum   = GetLittleEndianUInt32 (&Buffer[12]);

  // Compute the real number of tiles contained in the file
  NbTiles = TilesEndNum - TilesStartNum + 1;

  // Check the version number
  if (Version != 1)
  {
    printf ("Error: invalid ART file: invalid version number (%u)\n", Version);
    return false;
  }

  printf ("%u tiles declared in the ART header\n", NbTiles);

  // Extract sizes
  fread (Buffer, 1, NbTiles * 2, ArtFile);
  for (Ind = 0; Ind < NbTiles; Ind++)
    TilesList[Ind].XSize = GetLittleEndianUInt16 (&Buffer[Ind * 2]);
  fread (Buffer, 1, NbTiles * 2, ArtFile);
  for (Ind = 0; Ind < NbTiles; Ind++)
    TilesList[Ind].YSize = GetLittleEndianUInt16 (&Buffer[Ind * 2]);

  // Extract animation data
  fread (Buffer, 1, NbTiles * 4, ArtFile);
  for (Ind = 0; Ind < NbTiles; Ind++)
    TilesList[Ind].AnimData = GetLittleEndianUInt32 (&Buffer[Ind * 4]);

  // Compute offsets
  CrtOffset = 16 + NbTiles * (2 + 2 + 4);
  for (Ind = 0; Ind < NbTiles; Ind++)
  {
    TilesList[Ind].Offset = CrtOffset;
    CrtOffset += TilesList[Ind].XSize * TilesList[Ind].YSize;
  }

  return true;
}


/*
====================
LoadPalette

Load the color palette from the PALETTE.DAT file
====================
*/
static bool LoadPalette (void)
{
  // Variables
  FILE* PaletteFile;
  uint32 Ind;
  uint8 Color;

  // Open the file
  PaletteFile = fopen (PalFileName, "rb");
  if (PaletteFile == NULL) { PaletteFile = fopen (PalFileNameUc, "rb"); }
  if (PaletteFile == NULL)
  {
    printf ("Warning: can't open \"%s\"\n", PalFileName);
    return false;
  }

  // Read the palette
  if (fread (Palette, 1, PALETTE_SIZE, PaletteFile) != PALETTE_SIZE)
  {
    printf ("Warning: can't read the whole palette from \"%s\" file\n"
      , PalFileName);
    fclose (PaletteFile);
    return false;
  }

  // Convert it to TGA palette format
  for (Ind = 0; Ind < PALETTE_SIZE; Ind += 3)
  {
    Color = Palette[Ind];
    Palette[Ind] = (uint8)(Palette[Ind + 2] << 2);
    Palette[Ind + 1] <<= 2;
    Palette[Ind + 2] = (uint8)(Color << 2);
  }

  printf ("Palette successfully loaded from \"%s\"\n", PalFileName);
  fclose (PaletteFile);
  return true;
}


/*
====================
main

Main procedure
====================
*/
int main (int ArgC, char* ArgV [])
{
  // Header
  printf ("\n"
          "Art2Tga version " VERSION " by Mathieu Olivier\n"
          "========================================\n\n"
         );

  // Initial arguments check
  if (   ((ArgC != 2) && (ArgC != 3))
      || !strcmp(ArgV[1], "-h") || !strcmp(ArgV[1], "--help") )
  {
    printf (
      "Usage: %s [Build Game] <ART file|dumppal>\n"
      "\n  Extracts images from an ART file into \"tile????.tga\"\n"
      "    or dumps \"%s\" into \"%s\".\n"
      "\n  Examples:\n"
      "    art2tga tiles000.art\n"
      "    art2tga blood tiles015.art\n"
      "    art2tga dumppal\n"
      "    art2tga tekwar dumppal\n"
      "\n  Build Game shorthands (override the default palette):\n"
      "    duke    - (DEFAULT, if no \"%s\" is found)\n"
      "              Duke Nukem 3D (1.3D, and all later releases)\n"
      "              Redneck Rampage [Rides Again|Deer Huntin']\n"
      "              NAM, World War II GI, Liquidator\n"
      "              Electric Highways, Sonic the Hedgehog 3D, SilverballZ\n"
      "    blood   - Blood\n"
      "    corr8   - Corridor 8\n"
      "    exhumed - Exhumed / Powerslave\n"
      "    fate    - Fate\n"
      "    fury    - Ion Fury\n"
      "    ken     - KenBuild\n"
      "    lotp    - Legend of the Seven Paladins\n"
      "    paintb  - Extreme Paintbrawl\n"
      "    sw      - Shadow Warrior\n"
      "    tekwar  - William Shatner's TekWar\n"
      "    wgr2    - WGRealms 2\n"
      "    whv1    - Witchhaven\n"
      "    whv2    - Witchhaven 2\n"
      , ArgV[0], PalFileName, DumpFileName, PalFileName
    );
    return EXIT_FAILURE;
  }

  if (ArgC == 3)
  {
    if (       !strcmp(ArgV[1],    "duke") ) {
      printf ("Using %s palette\n", duke);
      memcpy (Palette,              dukePalette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "blood") ) {
      printf ("Using %s palette\n", blood);
      memcpy (Palette,              bloodPalette,   sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "corr8") ) {
      printf ("Using %s palette\n", corr8);
      memcpy (Palette,              corr8Palette,   sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "exhumed") ) {
      printf ("Using %s palette\n", exhumed);
      memcpy (Palette,              exhumedPalette, sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "fate") ) {
      printf ("Using %s palette\n", fate);
      memcpy (Palette,              fatePalette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "fury") ) {
      printf ("Using %s palette\n", fury);
      memcpy (Palette,              furyPalette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "ken") ) {
      printf ("Using %s palette\n", ken);
      memcpy (Palette,              kenPalette,     sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "lotp") ) {
      printf ("Using %s palette\n", lotp);
      memcpy (Palette,              lotpPalette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "paintb") ) {
      printf ("Using %s palette\n", paintb);
      memcpy (Palette,              paintbPalette,  sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "sw") ) {
      printf ("Using %s palette\n", sw);
      memcpy (Palette,              swPalette,      sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "tekwar") ) {
      printf ("Using %s palette\n", tekwar);
      memcpy (Palette,              tekwarPalette,  sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "wgr2") ) {
      printf ("Using %s palette\n", wgr2);
      memcpy (Palette,              wgr2Palette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "whv1") ) {
      printf ("Using %s palette\n", whv1);
      memcpy (Palette,              whv1Palette,    sizeof (Palette));
    } else if (!strcmp(ArgV[1],    "whv2") ) {
      printf ("Using %s palette\n", whv2);
      memcpy (Palette,              whv2Palette,    sizeof (Palette));
    } else {
      printf ("Bad Build game shorthand: %s\n", ArgV[1]);
      fflush( stdout );
      system(ArgV[0]);
      return EXIT_FAILURE;
    }
    ArtFileName = ArgV[2];
  }

  if (ArgC == 2)
  {
    ArtFileName = ArgV[1];

    // Try loading the color palette from file
    if (! LoadPalette ())
    {
      //printf ("Using %s palette instead.\n", blood);
      //memcpy (Palette, bloodPalette, sizeof (Palette));
      printf ("Using %s palette instead.\n", duke);
      memcpy (Palette, dukePalette, sizeof (Palette));
    }
    printf ("\n");
  }

  if (! strcmp(ArtFileName, "dumppal")) {
    DumpPalette();
    return EXIT_SUCCESS;
  }

  // Open the ART file
  ArtFile = fopen (ArtFileName, "rb");
  if (ArtFile == NULL)
  {
    printf ("Error: can't open file \"%s\"\n", ArtFileName);
    return EXIT_FAILURE;
  }

  // Extract all the pictures and create the INI file
  if (! GetPicturesList   () ||  // Read the ART header
      ! ExtractPictures   () ||  // Extract pictures from the ART file
      ! DumpAnimationData ()  )  // Dump animation data into "animdata.ini"
  {
    fclose (ArtFile);
    return EXIT_FAILURE;
  }

  fclose (ArtFile);
  return EXIT_SUCCESS;
}


/*
====================
SetLittleEndianUInt16

Set a uint16 into a little-endian ordered buffer
====================
*/
static void SetLittleEndianUInt16 (uint16 Integer, uint8* Buffer)
{
  Buffer[0] = (uint8)(Integer & 255);
  Buffer[1] = (uint8)(Integer >> 8);
}


/*
====================
SpawnTGA

Extract the picture number "PictureInd" and save it as PictureName
====================
*/
static bool SpawnTGA (uint32 TileInd, const char* PictureName)
{
  // Variables
  FILE* ImageFile;
  int32 XInd, YInd;
  uint8 TgaHeader [] =
  {
    0x00,                          // Number of Characters in Identification Field (0)
    0x01,                          // Color Map Type (true)
    0x01,                          // Image Type Code (1 -> uncompressed color mapped)
    0x00, 0x00, 0x00, 0x01, 0x18,  // Color Map Specification (from index 0; 256 colors; 24 bits each)
    0x00, 0x00, 0x00, 0x00,        // Origin of Image (0, 0)
    0x00, 0x00, 0x00, 0x00,        // Size of Image (TO BE FILLED. Bytes 12-13 and 14-15. little-endian)
    0x08,                          // Pixel size (8 bits)
    0x00                           // Flags (image described from bottom left; no alpha bits)
  };
  uint8* ImageBuffer;
  const uint32 PictureSize =
    TilesList[TileInd].XSize * TilesList[TileInd].YSize;

  // If the picture is empty, we skip it
  if (PictureSize == 0)
    return true;

  // Load the picture in a buffer
  fseek (ArtFile, TilesList[TileInd].Offset, SEEK_SET);
  ImageBuffer = malloc (PictureSize);
  if (ImageBuffer == NULL)
  {
    printf ("Error: can't allocate enough memory to load \"%s\"\n",
            PictureName);
    return false;
  }
  if (fread (ImageBuffer, 1, PictureSize, ArtFile) != PictureSize)
  {
    printf ("Error: can't read enough data in the ART file to load \"%s\"\n",
            PictureName);
    free (ImageBuffer);
    return false;
  }

  // Create the file
  ImageFile = fopen (PictureName, "wb");
  if (ImageFile == NULL)
  {
    printf ("Error: can't create file \"%s\"\n", PictureName);
    free (ImageBuffer);
    return false;
  }

  // Write the TGA header (including the palette)
  SetLittleEndianUInt16 (TilesList[TileInd].XSize, &TgaHeader[12]);
  SetLittleEndianUInt16 (TilesList[TileInd].YSize, &TgaHeader[14]);
  fwrite (TgaHeader, 1, sizeof (TgaHeader), ImageFile);
  fwrite (Palette, 1, PALETTE_SIZE, ImageFile);

  // Write the picture
    // Save the lines from bottom
  for (YInd = TilesList[TileInd].YSize - 1; YInd >= 0; YInd--)
    for (XInd = 0; XInd < TilesList[TileInd].XSize; XInd++)
      // ART files store pictures by columns, not by lines
      fwrite (&ImageBuffer[YInd + XInd * TilesList[TileInd].YSize], 1, 1,
              ImageFile);

  free (ImageBuffer);
  fclose (ImageFile);
  return true;
}


/*
====================
DumpPalette

Dump palette to C file, to be copied into this source code or palettes.h
====================
*/
static bool DumpPalette (void)
{
  FILE*  PaletteDataFile;
  uint32 i;

  PaletteDataFile = fopen (DumpFileName, "wt");
  if (PaletteDataFile == NULL)
  {
    printf ("Error: can't create file \"%s\"\n", DumpFileName);
    return false;
  }

  printf ("Creating \"%s\"...", DumpFileName);
  fflush (stdout);

  fprintf (PaletteDataFile,
           "// Dumped palette\n"
           "const uint8 dumpedPalette [PALETTE_SIZE] = {"
          );
  for (i = 0; i < (sizeof(Palette) -1); i++) {
    if ( (i % 12) == 0 ) {
      fprintf (PaletteDataFile, "\n");
    }
    fprintf (PaletteDataFile, " %3d,", Palette[i]);
  }
  fprintf (PaletteDataFile, " %3d", Palette[sizeof(Palette) -1]);
  fprintf (PaletteDataFile, "\n};\n");
  fclose  (PaletteDataFile);
  printf  (" done\n\n");
  return true;
}
