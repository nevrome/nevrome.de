#!/bin/bash

make

rm -r homepage/*
./build/homepage_builder "homepage/"
cp styles.css homepage/styles.css
cp KEX16_2016_08_13_Fontaene_cropped_small.jpg homepage/avatar.jpg