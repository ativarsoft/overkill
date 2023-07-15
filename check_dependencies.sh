#!/bin/bash
apt -qq list $(cat dependencies.list)

