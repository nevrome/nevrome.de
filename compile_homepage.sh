#!/bin/bash

make

rm -r homepage/*
./build/homepage_builder "homepage/"
cp styles.css homepage/styles.css
cp -r github-widget homepage/github-widget 