#!/bin/bash

make

./build/homepage_builder "homepage/"
cp styles.css homepage/styles.css